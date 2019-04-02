use crate::codegen::Generator;
use crate::codegen::eval::EvalContext;
use crate::codegen::attrs;
use crate::sections::FieldSection;
use crate::reporting::*;

use classfile::raw;
use classfile::consts as flags;

pub fn expand_field(gen: &mut Generator, field: FieldSection) -> Reported<raw::Field> {

    let mut reports = Reported::new();

    let mut cx = EvalContext::new(gen);

    let FieldSection {
        label: f_label,
        ident,
        name: f_name,
        descriptor: f_desc,
        meta: metas,
        ..
    } = field;

    let name = if let Some(name) = f_name {
        if let Some(label) = f_label {
            report_warning!(reports; "`field` label ignored in favor of second argument"; label);
        }
        report_try!(reports; cx.eval_into_utf8(name))
    } else {
        if let Some(label) = f_label {
            cx.gen.push_string_constant(label.name)
        } else {
            fatal_error!(reports; "`field` requires either a label or a second argument"; ident)
        }
    };

    let desc = report_try!(reports; cx.eval_into_utf8(f_desc));

    let mut flags = None;
    let mut attrs = Vec::new();

    for meta in metas.into_iter() {
        let span = meta.ident.span();
        match report_try!(reports; attrs::expand_field_attr(cx.gen, meta)) {
            attrs::FieldMeta::Flags(flag) => {
                if let Some(..) = flags {
                    report_warning!(reports; "`flags` instruction shadows previous one"; span);
                }
                flags = Some(flag)
            }
            attrs::FieldMeta::Attr(attr) => {
                attrs.push(attr);
            },
        }
    }

    let flags = if let Some(flags) = flags {
        flags
    } else {
        flags::field::Flags::new()
    };

    reports.complete(raw::Field {
        flags, name, descriptor: desc, attributes: attrs,
    })
}