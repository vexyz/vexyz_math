use gen_common::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_int_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}i", n),
        tpe: Type::I32,
        dims: n,
        macro_builder_name: format!("ivec{}", n),
        all_ordinals: &XYZW,
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

{template_methods}

{trait_display}

{op_index}

{op_index_mut}

{template_common_num_ops}

{op_neg}

{macro_builder}
",
    imports = format!("{}\n{}",
        vec_common::IMPORTS,
        format!("use {};", gen.bool_struct_name()),
    ),
    template_struct = vec_common::template_struct(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen,
        vec_common::template_common_num_postfix(gen)
    ),
    template_methods = vec_common::template_methods(gen, {
        let mut methods = vec_common::template_methods_compare(gen);
        methods.push(vec_common::method_dot(gen));
        methods
    }),
    trait_display = vec_common::trait_display(gen),
    op_index = vec_common::op_index(gen),
    op_index_mut = vec_common::op_index_mut(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen,
        vec_common::op_mul_vec(gen)
    ),
    op_neg = vec_common::op_neg(gen),
    macro_builder = vec_common::macro_builder(gen),
}}
