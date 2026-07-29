use sense_model::{MarkupContent, MarkupKind};
use sense_present::{
    DocumentationLineKind, DocumentationPlacement, DocumentationPlacementPreference,
    PresentationRequest, layout,
};

fn request(columns: u16) -> PresentationRequest {
    PresentationRequest {
        terminal_columns: columns,
        preferred_menu_width: 54,
        minimum_menu_width: 24,
        preference: DocumentationPlacementPreference::Auto,
        side_min_columns: 100,
        documentation_width_ratio: 0.45,
        documentation_max_rows: 8,
        documentation_padding: 1,
        bordered: false,
        render_markdown: true,
    }
}

#[test]
fn auto_layout_uses_side_only_when_both_panels_fit() {
    let content = MarkupContent {
        kind: MarkupKind::PlainText,
        value: "Restart one or more units.".into(),
    };
    let wide = layout(Some(&content), request(140));
    let panel = wide.documentation.unwrap();
    assert_eq!(panel.placement, DocumentationPlacement::Side);
    assert!(wide.menu_width >= 24);
    assert!(u32::from(wide.menu_width) + u32::from(panel.width) < 140);

    let narrow = layout(Some(&content), request(80));
    assert_eq!(
        narrow.documentation.unwrap().placement,
        DocumentationPlacement::Below
    );
}

#[test]
fn explicit_side_mode_ignores_the_auto_layout_threshold() {
    let content = MarkupContent {
        kind: MarkupKind::PlainText,
        value: "documentation".into(),
    };
    let mut configuration = request(80);
    configuration.preference = DocumentationPlacementPreference::Side;
    configuration.side_min_columns = 100;
    assert_eq!(
        layout(Some(&content), configuration)
            .documentation
            .unwrap()
            .placement,
        DocumentationPlacement::Side
    );
}

#[test]
fn markdown_is_parsed_into_terminal_roles_and_unicode_widths() {
    let content = MarkupContent {
        kind: MarkupKind::Markdown,
        value: "# Restart\n\nRestart **one** or more 界 units.\n\n- first\n- second".into(),
    };
    let mut configuration = request(80);
    configuration.preferred_menu_width = 28;
    let panel = layout(Some(&content), configuration).documentation.unwrap();
    assert_eq!(panel.lines[0].kind, DocumentationLineKind::Heading);
    assert_eq!(panel.lines[0].text, "Restart");
    assert!(
        panel
            .lines
            .iter()
            .any(|line| line.kind == DocumentationLineKind::ListItem)
    );
    assert!(panel.lines.iter().all(|line| line.cells <= 26));
}

#[test]
fn untrusted_controls_are_rendered_as_visible_text() {
    let content = MarkupContent {
        kind: MarkupKind::PlainText,
        value: "safe\u{1b}[31m".into(),
    };
    let panel = layout(Some(&content), request(80)).documentation.unwrap();
    assert_eq!(panel.lines[0].text, "safe\\u{1b}[31m");
}

#[test]
fn documentation_is_bounded_and_reports_truncation() {
    let content = MarkupContent {
        kind: MarkupKind::PlainText,
        value: (0..20)
            .map(|line| format!("line {line}"))
            .collect::<Vec<_>>()
            .join("\n"),
    };
    let mut configuration = request(80);
    configuration.documentation_max_rows = 3;
    let panel = layout(Some(&content), configuration).documentation.unwrap();
    assert_eq!(panel.lines.len(), 3);
    assert!(panel.truncated);
    assert!(panel.lines.last().unwrap().text.ends_with('…'));
}
