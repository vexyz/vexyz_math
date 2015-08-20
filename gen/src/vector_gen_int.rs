use common::*;
use util::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_int_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}i", n),
        tpe: Type::I32,
        dims: n,
        builder_macro_name: format!("ivec{}", n),
        all_ordinals: &vec_common::XYZW,
        doc_name: "vector".to_string(),
        val_name: "u".to_string(),
        quaternion_override: false,
    };
    template_main(gen)
}

fn template_main(gen: &VecGen) -> String { format! {"\
// Generated code.
{imports}

{struct_def}

{template_struct_impl}

{template_common_num_methods}

{op_index}

{template_common_num_ops}

{macro_builder}
",
    imports = vec_common::IMPORTS,
    struct_def = vec_common::struct_def(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen,
        vec_common::template_common_num_postfix(gen)
    ),
    op_index = vec_common::op_index(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen,
        vec_common::template_op_bin_vec(gen, "Mul", "mul", "*", &format!(
            "Performs component-wise multiplication of two vectors"))
    ),
    template_common_num_methods = vec_common::template_common_num_methods(gen),
    macro_builder = vec_common::macro_builder(gen),
}}
