use criterion::{BatchSize, Criterion, criterion_group, criterion_main};
use sense_model::{CompletionItem, NativeShell, TextEdit, TextRange};
use sense_rank::{RankConfig, RankSignals, Ranker};

const CANDIDATE_COUNT: usize = 10_000;

fn candidates() -> Vec<CompletionItem> {
    (0..CANDIDATE_COUNT)
        .map(|index| {
            let label = format!("command-option-{index:05}");
            let mut item = CompletionItem::native(
                format!("candidate-{index}"),
                NativeShell::Zsh,
                &label,
                TextEdit::new(TextRange::new(0, 0), label.as_str()),
                label.as_str(),
            );
            item.original_order = u32::try_from(index).expect("benchmark candidate index fits u32");
            item
        })
        .collect()
}

fn rank_candidates(criterion: &mut Criterion) {
    let ranker = Ranker::new(RankConfig::default());
    let signals = RankSignals::default();
    let mut group = criterion.benchmark_group("ranking");
    group.sample_size(50);
    group.bench_function("10k_fuzzy_candidates", |bencher| {
        bencher.iter_batched(
            candidates,
            |items| ranker.rank("cmdopt0999", items, None, &signals),
            BatchSize::LargeInput,
        );
    });
    group.finish();
}

criterion_group!(benches, rank_candidates);
criterion_main!(benches);
