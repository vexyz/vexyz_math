use common::*;
use util::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_bool_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}b", n),
        tpe: Type::Bool,
        dims: n,
        builder_macro_name: format!("bvec{}", n),
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

{op_index}

{template_boolean_ops}

{macro_builder}
",
    imports = vec_common::IMPORTS,
    struct_def = vec_common::struct_def(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen, "".to_string()),
    op_index = vec_common::op_index(gen),
    template_boolean_ops = template_boolean_ops(gen),
    macro_builder = vec_common::macro_builder(gen),
}}

fn template_boolean_ops(gen: &VecGen) -> String { format! { "\
{op_and_vec}

{op_and_scalar}

{op_or_vec}

{op_or_scalar}

{op_xor_vec}

{op_xor_scalar}

{op_not}",
    op_and_vec = vec_common::template_op_bin_vec(gen, "BitAnd", "bitand", "&",
        "Performs logical *and* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_or_vec = vec_common::template_op_bin_vec(gen, "BitOr", "bitor", "|",
        "Performs logical *or* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_xor_vec = vec_common::template_op_bin_vec(gen, "BitXor", "bitxor", "^",
        "Performs logical *xor* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_and_scalar = vec_common::template_op_bin_scalar(gen, "BitAnd", "bitand", "&",
        "Performs logical *and* between each component of a vector\
        \n    /// and a scalar"),
    
    op_or_scalar = vec_common::template_op_bin_scalar(gen, "BitOr", "bitor", "|",
        "Performs logical *or* between each component of a vector\
        \n    /// and a scalar"),
    
    op_xor_scalar = vec_common::template_op_bin_scalar(gen, "BitXor", "bitxor", "^",
        "Performs logical *xor* between each component of a vector\
        \n    /// and a scalar"),
    
    op_not = vec_common::template_op_unary(gen, "Not", "not", "!", "logical negation"),
}}
