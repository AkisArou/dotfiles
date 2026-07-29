//! Terminal-independent layout for Shell Sense completion documentation.

use pulldown_cmark::{Event, Parser, Tag, TagEnd};
use sense_model::{MarkupContent, MarkupKind};
use textwrap::Options;
use unicode_width::UnicodeWidthStr;

const MINIMUM_PANEL_WIDTH: u16 = 24;
const PANEL_GAP: u16 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentationPlacementPreference {
    Auto,
    Side,
    Below,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PresentationRequest {
    pub terminal_columns: u16,
    pub preferred_menu_width: u16,
    pub minimum_menu_width: u16,
    pub preference: DocumentationPlacementPreference,
    pub side_min_columns: u16,
    pub documentation_width_ratio: f32,
    pub documentation_max_rows: u16,
    pub side_viewport_rows: u16,
    pub documentation_offset: usize,
    pub documentation_padding: u16,
    pub documentation_scrollbar: bool,
    pub bordered: bool,
    pub render_markdown: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentationPlacement {
    Side,
    Below,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentationLineKind {
    Text,
    Heading,
    Code,
    ListItem,
    Quote,
    Separator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentationLine {
    pub text: String,
    pub cells: u16,
    pub kind: DocumentationLineKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentationPanel {
    pub placement: DocumentationPlacement,
    pub width: u16,
    pub lines: Vec<DocumentationLine>,
    pub viewport_rows: usize,
    pub offset: usize,
    pub total_lines: usize,
    pub scrollbar: bool,
    pub has_previous: bool,
    pub has_next: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PresentationLayout {
    pub menu_width: u16,
    pub documentation: Option<DocumentationPanel>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DocumentationBlock {
    text: String,
    kind: DocumentationLineKind,
}

#[must_use]
pub fn layout(
    documentation: Option<&MarkupContent>,
    request: PresentationRequest,
) -> PresentationLayout {
    let terminal_width = request.terminal_columns.saturating_sub(1).max(1);
    let minimum_menu = request.minimum_menu_width.min(terminal_width);
    let preferred_menu = request
        .preferred_menu_width
        .clamp(minimum_menu, terminal_width);
    let Some(documentation) = documentation.filter(|content| !content.value.trim().is_empty())
    else {
        return PresentationLayout {
            menu_width: preferred_menu,
            documentation: None,
        };
    };

    let placement = select_placement(request, terminal_width, minimum_menu);
    let (menu_width, documentation_width) = match placement {
        DocumentationPlacement::Side => side_widths(request, terminal_width, minimum_menu),
        DocumentationPlacement::Below => (preferred_menu, preferred_menu),
    };
    let decoration_cells = request
        .documentation_padding
        .saturating_mul(2)
        .saturating_add(u16::from(request.bordered).saturating_mul(2));
    let undecorated_width = documentation_width.saturating_sub(decoration_cells).max(1);
    let blocks = document_blocks(documentation, request.render_markdown);
    let maximum_rows = usize::from(
        match placement {
            DocumentationPlacement::Side => request.side_viewport_rows,
            DocumentationPlacement::Below => request.documentation_max_rows,
        }
        .max(1),
    );
    let mut all_lines = wrap_blocks(&blocks, usize::from(undecorated_width));
    let scrollbar =
        request.documentation_scrollbar && all_lines.len() > maximum_rows && undecorated_width > 1;
    if scrollbar {
        all_lines = wrap_blocks(&blocks, usize::from(undecorated_width - 1));
    }
    let total_lines = all_lines.len();
    let maximum_offset = total_lines.saturating_sub(maximum_rows);
    let offset = request.documentation_offset.min(maximum_offset);
    let lines: Vec<_> = all_lines
        .into_iter()
        .skip(offset)
        .take(maximum_rows)
        .collect();
    let viewport_rows = match placement {
        DocumentationPlacement::Side => maximum_rows,
        DocumentationPlacement::Below => lines.len(),
    };
    PresentationLayout {
        menu_width,
        documentation: Some(DocumentationPanel {
            placement,
            width: documentation_width,
            lines,
            viewport_rows,
            offset,
            total_lines,
            scrollbar,
            has_previous: offset > 0,
            has_next: offset < maximum_offset,
        }),
    }
}

fn select_placement(
    request: PresentationRequest,
    terminal_width: u16,
    minimum_menu: u16,
) -> DocumentationPlacement {
    let minimum_side_width = minimum_menu
        .saturating_add(PANEL_GAP)
        .saturating_add(MINIMUM_PANEL_WIDTH);
    let side_fits = terminal_width >= minimum_side_width;
    match request.preference {
        DocumentationPlacementPreference::Below => DocumentationPlacement::Below,
        DocumentationPlacementPreference::Side if side_fits => DocumentationPlacement::Side,
        DocumentationPlacementPreference::Auto
            if side_fits && terminal_width >= request.side_min_columns =>
        {
            DocumentationPlacement::Side
        }
        DocumentationPlacementPreference::Side | DocumentationPlacementPreference::Auto => {
            DocumentationPlacement::Below
        }
    }
}

fn side_widths(request: PresentationRequest, terminal_width: u16, minimum_menu: u16) -> (u16, u16) {
    let maximum_documentation = terminal_width
        .saturating_sub(PANEL_GAP)
        .saturating_sub(minimum_menu);
    let desired_documentation =
        num_traits::cast((f32::from(terminal_width) * request.documentation_width_ratio).round())
            .unwrap_or(maximum_documentation);
    let documentation_width = desired_documentation
        .max(MINIMUM_PANEL_WIDTH)
        .min(maximum_documentation);
    let available_menu = terminal_width
        .saturating_sub(PANEL_GAP)
        .saturating_sub(documentation_width);
    let menu_width = request
        .preferred_menu_width
        .clamp(minimum_menu, available_menu);
    (menu_width, documentation_width)
}

fn document_blocks(content: &MarkupContent, render_markdown: bool) -> Vec<DocumentationBlock> {
    if content.kind == MarkupKind::PlainText || !render_markdown {
        return plain_blocks(&content.value);
    }
    markdown_blocks(&content.value)
}

fn plain_blocks(value: &str) -> Vec<DocumentationBlock> {
    sanitize(value)
        .split('\n')
        .map(|line| DocumentationBlock {
            text: line.to_owned(),
            kind: DocumentationLineKind::Text,
        })
        .collect()
}

fn markdown_blocks(value: &str) -> Vec<DocumentationBlock> {
    let mut blocks = Vec::new();
    let mut current = String::new();
    let mut kind = DocumentationLineKind::Text;
    let mut code_depth = 0_u16;
    let mut quote_depth = 0_u16;
    let mut item_depth = 0_u16;
    for event in Parser::new(value) {
        match event {
            Event::Start(Tag::Heading { .. }) => kind = DocumentationLineKind::Heading,
            Event::Start(Tag::CodeBlock(_)) => {
                flush_block(&mut blocks, &mut current, kind);
                code_depth = code_depth.saturating_add(1);
                kind = DocumentationLineKind::Code;
            }
            Event::Start(Tag::BlockQuote(_)) => {
                quote_depth = quote_depth.saturating_add(1);
                kind = DocumentationLineKind::Quote;
            }
            Event::Start(Tag::Item) => {
                item_depth = item_depth.saturating_add(1);
                kind = DocumentationLineKind::ListItem;
            }
            Event::End(
                TagEnd::Paragraph | TagEnd::Heading(_) | TagEnd::Item | TagEnd::CodeBlock,
            ) => {
                flush_block(&mut blocks, &mut current, kind);
                code_depth = code_depth.saturating_sub(1);
                item_depth = item_depth.saturating_sub(1);
                kind = inherited_kind(code_depth, quote_depth, item_depth);
            }
            Event::End(TagEnd::BlockQuote(_)) => {
                flush_block(&mut blocks, &mut current, kind);
                quote_depth = quote_depth.saturating_sub(1);
                kind = inherited_kind(code_depth, quote_depth, item_depth);
            }
            Event::Text(text) | Event::Code(text) | Event::InlineMath(text) => {
                current.push_str(&sanitize(&text));
            }
            Event::DisplayMath(text) => {
                flush_block(&mut blocks, &mut current, kind);
                blocks.push(DocumentationBlock {
                    text: sanitize(&text),
                    kind: DocumentationLineKind::Code,
                });
            }
            Event::SoftBreak => current.push(if code_depth > 0 { '\n' } else { ' ' }),
            Event::HardBreak => flush_block(&mut blocks, &mut current, kind),
            Event::Rule => {
                flush_block(&mut blocks, &mut current, kind);
                blocks.push(DocumentationBlock {
                    text: String::new(),
                    kind: DocumentationLineKind::Separator,
                });
            }
            Event::TaskListMarker(checked) => {
                current.push_str(if checked { "[x] " } else { "[ ] " });
            }
            Event::FootnoteReference(reference) => {
                current.push('[');
                current.push_str(&sanitize(&reference));
                current.push(']');
            }
            Event::Html(_) | Event::InlineHtml(_) | Event::Start(_) | Event::End(_) => {}
        }
    }
    flush_block(&mut blocks, &mut current, kind);
    blocks
}

const fn inherited_kind(
    code_depth: u16,
    quote_depth: u16,
    item_depth: u16,
) -> DocumentationLineKind {
    if code_depth > 0 {
        DocumentationLineKind::Code
    } else if item_depth > 0 {
        DocumentationLineKind::ListItem
    } else if quote_depth > 0 {
        DocumentationLineKind::Quote
    } else {
        DocumentationLineKind::Text
    }
}

fn flush_block(
    blocks: &mut Vec<DocumentationBlock>,
    current: &mut String,
    kind: DocumentationLineKind,
) {
    if current.is_empty() {
        return;
    }
    blocks.push(DocumentationBlock {
        text: std::mem::take(current),
        kind,
    });
}

fn wrap_blocks(blocks: &[DocumentationBlock], width: usize) -> Vec<DocumentationLine> {
    let mut lines = Vec::new();
    for block in blocks {
        if block.kind == DocumentationLineKind::Separator {
            lines.push(DocumentationLine {
                text: "─".repeat(width),
                cells: u16::try_from(width).unwrap_or(u16::MAX),
                kind: block.kind,
            });
            continue;
        }
        let options = Options::new(width).break_words(true);
        for source_line in block.text.split('\n') {
            if source_line.is_empty() {
                lines.push(documentation_line(String::new(), block.kind));
                continue;
            }
            lines.extend(
                textwrap::wrap(source_line, &options)
                    .into_iter()
                    .map(|line| documentation_line(line.into_owned(), block.kind)),
            );
        }
    }
    lines
}

fn documentation_line(text: String, kind: DocumentationLineKind) -> DocumentationLine {
    DocumentationLine {
        cells: u16::try_from(UnicodeWidthStr::width(text.as_str())).unwrap_or(u16::MAX),
        text,
        kind,
    }
}

fn sanitize(value: &str) -> String {
    let mut output = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\n' | '\r' => output.push('\n'),
            '\t' => output.push_str("    "),
            character if character.is_control() => output.extend(character.escape_default()),
            character => output.push(character),
        }
    }
    output
}
