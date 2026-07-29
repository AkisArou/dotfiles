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
        side_viewport_rows: 6,
        documentation_offset: 0,
        documentation_padding: 1,
        documentation_scrollbar: true,
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
fn documentation_is_a_bounded_scrollable_viewport() {
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
    assert_eq!(panel.offset, 0);
    assert_eq!(panel.total_lines, 20);
    assert!(!panel.has_previous);
    assert!(panel.has_next);
    assert_eq!(panel.lines.last().unwrap().text, "line 2");
    assert_eq!(panel.viewport_rows, 3);
    assert!(panel.scrollbar);

    configuration.documentation_offset = 19;
    let panel = layout(Some(&content), configuration).documentation.unwrap();
    assert_eq!(panel.offset, 17);
    assert!(panel.has_previous);
    assert!(!panel.has_next);
    assert_eq!(panel.lines.first().unwrap().text, "line 17");
    assert_eq!(panel.lines.last().unwrap().text, "line 19");
}

#[test]
fn side_documentation_matches_the_menu_height_and_reserves_its_scrollbar() {
    let content = MarkupContent {
        kind: MarkupKind::PlainText,
        value: (0..20)
            .map(|line| format!("line {line}"))
            .collect::<Vec<_>>()
            .join("\n"),
    };
    let mut configuration = request(140);
    configuration.documentation_max_rows = 14;
    configuration.side_viewport_rows = 10;
    configuration.documentation_padding = 0;
    let panel = layout(Some(&content), configuration).documentation.unwrap();

    assert_eq!(panel.placement, DocumentationPlacement::Side);
    assert_eq!(panel.viewport_rows, 10);
    assert_eq!(panel.lines.len(), 10);
    assert!(panel.scrollbar);
    assert!(panel.lines.iter().all(|line| line.cells < panel.width));
}
