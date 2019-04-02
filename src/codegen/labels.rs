use crate::ast::*;
use crate::codegen::Generator;
use crate::reporting::*;
use crate::sections::*;

#[derive(Clone, Copy, Debug)]
pub enum LabelKind {
    Constant(u32),
    Item,
    Reserved,
    Bootstrap(u16),
    Code(u16),
}

pub fn expand_labels(gen: &mut Generator, section: &ClassSection) -> Reported<()> {

    let mut reports = Reported::new();

    macro_rules! insert_label {
        ($label:expr, $kind:expr) => {
            if gen.labels.contains_key(&$label.name) {
                // TODO: point out which the label being shadowed
                // is
                report_error!(reports; "duplicate label"; $label.span);
            } else {
                gen.labels.insert($label.name.clone(), $kind);
            }
        }
    }
    
    // FIXME: better way to reserve labels
    gen.labels.insert("this".into(), LabelKind::Reserved);
    gen.labels.insert("super".into(), LabelKind::Reserved);

    if let Some(ref label) = section.label {
        report_warning!(reports; "unused label on `class`"; label.span);
    }

    let mut index = 0;
    for meta in section.top_level.iter() {
        if let Some(ref label) = meta.label {
            match meta.body {
                MetaInstruction::Bootstrap(_) => {
                    insert_label!(label, LabelKind::Bootstrap(index));
                    index += 1;
                }
                _ => report_warning!(reports; "unused label on `{}`", meta.ident.name; label.span)
            }
        }
    }

    let mut index = 0;
    for constant in section.constants.iter() {
        if let Some(ref label) = constant.label {
            insert_label!(label, LabelKind::Constant(index + 1));
        }
        index += match constant.body {
            ConstantInstruction::Long(..) |
            ConstantInstruction::Double(..) => 2,
            _ => 1,
        }
    }
    gen.declared_count = index as usize + 1;
    
    for field in section.fields.iter() {
        if let Some(ref label) = field.label {
            insert_label!(label, LabelKind::Item);
        }
        for meta in field.meta.iter() {
            if let Some(ref label) = meta.label {
                report_warning!(reports; "unused label on `{}`", meta.ident.name; label);
            }
        }
    }

    for method in section.methods.iter() {
        if let Some(ref label) = method.label {
            insert_label!(label, LabelKind::Item);
        }
        for meta in method.meta.iter() {
            if let Some(ref label) = meta.label {
                report_warning!(reports; "unused label on `{}`", meta.ident.name; label);
            }
        }
        // we'll deal with code labels in method expansion, since
        // those are method-scoped.
    }
    
    reports.complete(())
}