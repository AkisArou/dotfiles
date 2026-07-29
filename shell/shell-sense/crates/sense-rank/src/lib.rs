//! Stable, group-aware completion ranking built on Frizbee.
//!
//! This crate owns textual matching and deterministic merge/rank policy. It
//! never interprets or reconstructs shell-owned insertion fingerprints.

use std::collections::HashMap;

use frizbee::{CaseMatching, Config as FrizbeeConfig, Matcher};
use sense_config::{CaseMode, MatchingConfig, TypoMode};
use sense_model::{
    CompletionItem, CompletionKind, Confidence, DocumentationState, GroupId, ItemId, MatchResult,
    RawBytes, SourceId, TextRange,
};

const SCORE_SCALE: i64 = 1_024;
const EXACT_BOOST: i64 = 100_000_000;
const PREFIX_BOOST: i64 = 10_000_000;
const SOURCE_PRIORITY_SCALE: i64 = 10_000;
const PROVIDER_RELEVANCE_SCALE: i64 = 1_000;

/// Bounded policy used for one ranking operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RankConfig {
    pub case: CaseMode,
    pub typos: TypoMode,
    pub max_typos: u16,
    pub typo_min_query_chars: u16,
    pub preserve_groups: bool,
    pub max_results: usize,
}

impl RankConfig {
    #[must_use]
    pub fn from_matching(config: &MatchingConfig) -> Self {
        Self {
            case: config.case,
            typos: config.typos,
            max_typos: config.max_typos,
            typo_min_query_chars: config.typo_min_query_chars,
            preserve_groups: config.preserve_groups,
            max_results: config.max_results as usize,
        }
    }
}

impl Default for RankConfig {
    fn default() -> Self {
        Self::from_matching(&MatchingConfig::default())
    }
}

/// Non-textual signals supplied by the daemon and semantic providers.
#[derive(Debug, Clone, Default)]
pub struct RankSignals {
    pub source_priorities: HashMap<SourceId, i32>,
    pub item_scores: HashMap<ItemId, i32>,
}

/// Deterministic result of merge, filtering, and ranking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RankedCandidates {
    pub items: Vec<CompletionItem>,
    pub selected_index: Option<usize>,
    pub matched_before_limit: usize,
    pub duplicates_merged: usize,
}

/// Reusable policy object. Frizbee matchers remain request-local because the
/// query changes between edits while the candidate set can remain cached.
#[derive(Debug, Clone)]
pub struct Ranker {
    config: RankConfig,
}

impl Ranker {
    #[must_use]
    pub const fn new(config: RankConfig) -> Self {
        Self { config }
    }

    #[must_use]
    pub const fn config(&self) -> &RankConfig {
        &self.config
    }

    /// Merge equivalent candidates, fuzzy-filter them, and rank them.
    ///
    /// Group order is the order in which groups first appeared. When group
    /// preservation is enabled, candidates are only ranked against siblings
    /// in the same group.
    #[must_use]
    pub fn rank(
        &self,
        query: &str,
        items: Vec<CompletionItem>,
        selected: Option<&ItemId>,
        signals: &RankSignals,
    ) -> RankedCandidates {
        let (items, duplicates_merged) = deduplicate(items, self.config.preserve_groups, signals);
        let matched_before_limit;
        let mut ranked = if self.config.preserve_groups {
            let buckets = group_in_first_seen_order(items);
            let mut remaining = self.config.max_results;
            let mut output = Vec::new();
            let mut matched = 0;
            for bucket in buckets {
                let (bucket, bucket_matches) = self.rank_bucket(query, bucket, signals, remaining);
                matched += bucket_matches;
                remaining = remaining.saturating_sub(bucket.len());
                output.extend(bucket);
            }
            matched_before_limit = matched;
            output
        } else {
            let (output, matches) =
                self.rank_bucket(query, items, signals, self.config.max_results);
            matched_before_limit = matches;
            output
        };

        // Truncating to zero should release candidate-owned provider payloads
        // immediately instead of retaining capacity in a session cache.
        if self.config.max_results == 0 {
            ranked = Vec::new();
        }
        let selected_index = selected
            .and_then(|selected| ranked.iter().position(|item| item.id == *selected))
            .or((!ranked.is_empty()).then_some(0));

        RankedCandidates {
            items: ranked,
            selected_index,
            matched_before_limit,
            duplicates_merged,
        }
    }

    fn rank_bucket(
        &self,
        query: &str,
        items: Vec<CompletionItem>,
        signals: &RankSignals,
        limit: usize,
    ) -> (Vec<CompletionItem>, usize) {
        if query.is_empty() {
            let mut items = rank_without_query(items, signals);
            let matched = items.len();
            items.truncate(limit);
            return (items, matched);
        }

        let haystacks: Vec<&str> = items
            .iter()
            .map(|item| item.filter_text.as_deref().unwrap_or(&item.label))
            .collect();
        let frizbee_config = self.frizbee_config(query);
        let mut matcher = Matcher::new(query, &frizbee_config);
        let mut entries: Vec<RankEntry> = matcher
            .match_list(&haystacks)
            .into_iter()
            .map(|matched| {
                let index = matched.index as usize;
                let text = haystacks[index];
                let exact = text_eq(text, query, self.config.case);
                let prefix = text_starts_with(text, query, self.config.case);
                RankEntry {
                    index,
                    score: textual_score(matched.score, exact, prefix)
                        + contextual_score(&items[index], signals),
                    exact,
                    prefix,
                }
            })
            .collect();

        entries.sort_by(|left, right| {
            right
                .score
                .cmp(&left.score)
                .then_with(|| right.exact.cmp(&left.exact))
                .then_with(|| right.prefix.cmp(&left.prefix))
                .then_with(|| {
                    items[left.index]
                        .original_order
                        .cmp(&items[right.index].original_order)
                })
                .then_with(|| left.index.cmp(&right.index))
        });
        let match_count = entries.len();
        entries.truncate(limit);

        let items = entries
            .into_iter()
            .map(|entry| {
                let mut item = items[entry.index].clone();
                let matcher_index = u32::try_from(entry.index)
                    .expect("Frizbee rejects candidate lists larger than u32::MAX");
                let mut indices = matcher
                    .match_one_indices(haystacks[entry.index], matcher_index)
                    .map_or_else(Vec::new, |mut matched| {
                        matched.indices.reverse();
                        matched.indices
                    });
                indices.dedup();
                item.match_result = Some(MatchResult {
                    score: entry.score,
                    indices,
                    exact: entry.exact,
                    prefix: entry.prefix,
                });
                item
            })
            .collect();
        (items, match_count)
    }

    fn frizbee_config(&self, query: &str) -> FrizbeeConfig {
        FrizbeeConfig::default()
            .casing(match self.config.case {
                CaseMode::Smart => CaseMatching::Smart,
                CaseMode::Sensitive => CaseMatching::Respect,
                CaseMode::Insensitive => CaseMatching::Ignore,
            })
            .max_typos(Some(self.allowed_typos(query)))
    }

    fn allowed_typos(&self, query: &str) -> u16 {
        match self.config.typos {
            TypoMode::Off => 0,
            TypoMode::Fixed => self.config.max_typos,
            TypoMode::Adaptive => {
                let length = query.chars().count();
                if length < usize::from(self.config.typo_min_query_chars) {
                    0
                } else {
                    self.config
                        .max_typos
                        .min(u16::try_from(length / 4).unwrap_or(u16::MAX).max(1))
                }
            }
        }
    }
}

#[derive(Debug)]
struct RankEntry {
    index: usize,
    score: i64,
    exact: bool,
    prefix: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DedupKey {
    group: Option<GroupId>,
    range: TextRange,
    insertion: InsertionIdentity,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum InsertionIdentity {
    Edit(RawBytes),
    Opaque(SourceId, ItemId),
}

fn deduplicate(
    items: Vec<CompletionItem>,
    preserve_groups: bool,
    signals: &RankSignals,
) -> (Vec<CompletionItem>, usize) {
    let mut indices = HashMap::<DedupKey, usize>::new();
    let mut deduplicated: Vec<CompletionItem> = Vec::with_capacity(items.len());
    let mut merged = 0;

    for item in items {
        let key = dedup_key(&item, preserve_groups);
        if let Some(&index) = indices.get(&key) {
            merged += 1;
            if primary_score(&item, signals) > primary_score(&deduplicated[index], signals) {
                let previous = std::mem::replace(&mut deduplicated[index], item);
                merge_metadata(&mut deduplicated[index], previous);
            } else {
                merge_metadata(&mut deduplicated[index], item);
            }
        } else {
            let index = deduplicated.len();
            indices.insert(key, index);
            deduplicated.push(item);
        }
    }
    (deduplicated, merged)
}

fn dedup_key(item: &CompletionItem, preserve_groups: bool) -> DedupKey {
    let insertion = if item.edit.new_text.is_empty() {
        InsertionIdentity::Opaque(item.source.clone(), item.id.clone())
    } else {
        InsertionIdentity::Edit(item.edit.new_text.clone())
    };
    DedupKey {
        group: preserve_groups.then(|| item.group.clone()).flatten(),
        range: item.edit.range,
        insertion,
    }
}

fn merge_metadata(primary: &mut CompletionItem, secondary: CompletionItem) {
    primary.tags |= secondary.tags;
    primary.capabilities |= secondary.capabilities;
    primary.confidence = primary.confidence.max(secondary.confidence);
    primary.provider_relevance = primary.provider_relevance.max(secondary.provider_relevance);
    primary.original_order = primary.original_order.min(secondary.original_order);

    if primary.kind == CompletionKind::Text && secondary.kind != CompletionKind::Text {
        primary.kind = secondary.kind;
    }
    if primary.label_detail.is_none() {
        primary.label_detail = secondary.label_detail;
    }
    if primary.filter_text.is_none() {
        primary.filter_text = secondary.filter_text;
    }
    if primary.sort_text.is_none() {
        primary.sort_text = secondary.sort_text;
    }
    if primary.detail.is_none() {
        primary.detail = secondary.detail;
    }
    if documentation_rank(&secondary.documentation) > documentation_rank(&primary.documentation) {
        primary.documentation = secondary.documentation;
    }
    for character in secondary.commit_characters {
        if !primary.commit_characters.contains(&character) {
            primary.commit_characters.push(character);
        }
    }
}

const fn documentation_rank(documentation: &DocumentationState) -> u8 {
    match documentation {
        DocumentationState::None => 0,
        DocumentationState::Unresolved => 1,
        DocumentationState::Resolved(_) => 2,
    }
}

fn group_in_first_seen_order(items: Vec<CompletionItem>) -> Vec<Vec<CompletionItem>> {
    let mut group_indices = HashMap::<Option<GroupId>, usize>::new();
    let mut groups: Vec<Vec<CompletionItem>> = Vec::new();
    for item in items {
        let key = item.group.clone();
        let index = *group_indices.entry(key).or_insert_with(|| {
            groups.push(Vec::new());
            groups.len() - 1
        });
        groups[index].push(item);
    }
    groups
}

fn rank_without_query(
    mut items: Vec<CompletionItem>,
    signals: &RankSignals,
) -> Vec<CompletionItem> {
    items.sort_by(|left, right| {
        contextual_score(right, signals)
            .cmp(&contextual_score(left, signals))
            .then_with(|| left.original_order.cmp(&right.original_order))
    });
    for item in &mut items {
        item.match_result = None;
    }
    items
}

fn textual_score(score: u16, exact: bool, prefix: bool) -> i64 {
    i64::from(score) * SCORE_SCALE
        + if exact { EXACT_BOOST } else { 0 }
        + if prefix { PREFIX_BOOST } else { 0 }
}

fn contextual_score(item: &CompletionItem, signals: &RankSignals) -> i64 {
    i64::from(
        signals
            .source_priorities
            .get(&item.source)
            .copied()
            .unwrap_or_default(),
    ) * SOURCE_PRIORITY_SCALE
        + i64::from(item.provider_relevance) * PROVIDER_RELEVANCE_SCALE
        + i64::from(
            signals
                .item_scores
                .get(&item.id)
                .copied()
                .unwrap_or_default(),
        )
}

fn primary_score(item: &CompletionItem, signals: &RankSignals) -> (u8, i64, u8) {
    (
        confidence_rank(item.confidence),
        contextual_score(item, signals),
        metadata_richness(item),
    )
}

const fn confidence_rank(confidence: Confidence) -> u8 {
    match confidence {
        Confidence::Advisory => 0,
        Confidence::Inferred => 1,
        Confidence::Partial => 2,
        Confidence::Authoritative => 3,
    }
}

fn metadata_richness(item: &CompletionItem) -> u8 {
    u8::from(item.detail.is_some())
        + u8::from(item.label_detail.is_some())
        + documentation_rank(&item.documentation)
        + u8::from(item.kind != CompletionKind::Text)
}

fn respects_case(query: &str, case: CaseMode) -> bool {
    match case {
        CaseMode::Sensitive => true,
        CaseMode::Insensitive => false,
        CaseMode::Smart => query.chars().any(char::is_uppercase),
    }
}

fn text_eq(text: &str, query: &str, case: CaseMode) -> bool {
    if respects_case(query, case) {
        text == query
    } else {
        text.to_lowercase() == query.to_lowercase()
    }
}

fn text_starts_with(text: &str, query: &str, case: CaseMode) -> bool {
    if respects_case(query, case) {
        text.starts_with(query)
    } else {
        text.to_lowercase().starts_with(&query.to_lowercase())
    }
}
