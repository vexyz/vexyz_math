use gen_common::*;
use util::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_usize_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}u", n),
        tpe: Type::Usize,
        dims: n,
        macro_builder_name: format!("uvec{}", n),
        all_ordinals: &vec_common::XYZW,
        doc_name: "vector".to_string(),
        val_name: "u".to_string(),
        quaternion_override: false,
    };
    template_file(gen)
}

fn template_file(gen: &VecGen) -> String { format! {"\
// Generated code.
{imports}

{template_struct}

{template_struct_impl}

{template_common_num_methods}

{op_index}

{template_common_num_ops}

{macro_builder}
",
    imports = vec_common::IMPORTS,
    template_struct = vec_common::template_struct(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen,
        vec_common::template_common_num_postfix(gen)
    ),
    op_index = vec_common::op_index(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen,
        vec_common::op_mul_vec(gen)
    ),
    template_common_num_methods = vec_common::template_common_num_methods(gen),
    macro_builder = vec_common::macro_builder(gen),
}}
