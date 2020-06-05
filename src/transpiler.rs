use rslua::ast::*;
use rslua::ast_walker::*;
use rslua::lexer::Lexer;
use rslua::parser::Parser;
use rslua::types::*;
use std::mem;
use std::str;

pub struct Transpiler {
    output: String,
    indent: usize,
    depth: usize,
}

#[allow(dead_code)]
impl Transpiler {
    pub fn new() -> Self {
        Transpiler {
            output: String::new(),
            indent: 2,
            depth: 0,
        }
    }

    pub fn run(&mut self, block: &Block) -> String {
        self.output.clear();
        ast_walker::walk_block(block, self);
        mem::replace(&mut self.output, String::new())
    }

    pub fn get_expr(&mut self, expr: &Expr) -> String {
        ast_walker::walk_expr(expr, self);
        mem::replace(&mut self.output, String::new())
    }

    fn append(&mut self, content: &str) {
        self.output.push_str(content);
    }

    fn incline(&mut self) {
        self.output.push_str("\n");
        self.output.push_str(&" ".repeat(self.depth * self.indent));
    }

    fn space(&mut self) {
        self.output.push_str(" ");
    }

    fn append_space(&mut self, content: &str) {
        self.append(content);
        self.space();
    }

    fn space_append(&mut self, content: &str) {
        self.space();
        self.append(content);
    }

    fn space_append_space(&mut self, content: &str) {
        self.space();
        self.append(content);
        self.space();
    }

    fn append_inc(&mut self, content: &str) {
        self.append(content);
        self.incline();
    }

    fn end(&mut self) {
        self.leave_scope();
        self.append("}");
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    fn leave_scope(&mut self) {
        self.depth -= 1;
        for _i in 0..self.indent {
            self.output.pop();
        }
    }
}

impl AstVisitor for Transpiler {
    fn stat_sep(&mut self) {
        self.append_inc(";");
    }

    fn begin_if(&mut self, _cond: &Expr) -> bool {
        self.append("if (");
        false
    }

    fn then(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.append_inc(") {");
        false
    }

    fn begin_else_if(&mut self, _cond: &Expr) -> bool {
        self.leave_scope();
        self.append("} else if (");
        false
    }

    fn begin_else(&mut self, _block: &Block) -> bool {
        self.leave_scope();
        self.append("} else {");
        self.enter_scope();
        self.incline();
        false
    }

    fn end_if(&mut self) {
        self.end();
    }

    fn begin_while(&mut self, _cond: &Expr) -> bool {
        self.append("while (");
        false
    }

    fn begin_while_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.append_inc(") {");
        false
    }

    fn end_while(&mut self) {
        self.end();
    }

    fn begin_do_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.append_inc("{");
        false
    }

    fn end_do_block(&mut self) {
        self.end();
    }

    fn for_num(&mut self, fornum: &ForNum) -> bool {
        let init = Transpiler::new().get_expr(&fornum.init);
        let limit = Transpiler::new().get_expr(&fornum.limit);
        let step = if let Some(step_expr) = &fornum.step {
            Transpiler::new().get_expr(step_expr)
        } else {
            "1".to_string()
        };
        self.append(&format!(
            "for (let {var} = {init}; {var} <= {limit}; {var} += {step})",
            var = fornum.var,
            init = init,
            limit = limit,
            step = step
        ));
        true
    }

    fn for_list(&mut self, forlist: &ForList) -> bool {
        todo!()
    }

    fn begin_for_block(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.space();
        self.append_inc("{");
        false
    }

    fn end_for(&mut self) {
        self.end();
    }

    fn begin_repeat(&mut self, _block: &Block) -> bool {
        self.enter_scope();
        self.append_inc("do {");
        false
    }

    fn until(&mut self) {
        self.leave_scope();
        self.incline();
        self.append_space("} while (");
    }

    fn end_repeat(&mut self) {
        self.append_inc(");")
    }

    fn func(&mut self, funcstat: &FuncStat) {
        let func_name = &funcstat.func_name;
        let mut fields = func_name.fields.iter();
        if let Some(name) = fields.next() {
            self.append(name);
            if func_name.fields.len() == 1 && funcstat.func_type == FuncType::Local {
                self.append_space("let");
            }
            while let Some(name) = fields.next() {
                self.append(".");
                self.append(name);
            }
            if let Some(method) = &func_name.method {
                self.append(".");
                self.append(method);
            }
            self.space_append_space("=");
            self.append_space("function");
        }
    }

    fn local_stat(&mut self, stat: &LocalStat) {
        self.append_space("let");
        let destruct = stat.names.len() > 1 && stat.exprs.len() > 0;
        if destruct {
            self.append_space("[");
        }
        for (n, name) in stat.names.iter().enumerate() {
            self.append(name);
            if n < stat.names.len() - 1 {
                self.append(", ");
            }
        }
        if destruct {
            self.space_append_space("]");
        }
        if stat.exprs.len() > 0 {
            self.append_space("=");
            if destruct {
                self.append_space("[");
            }
            ast_walker::walk_exprlist(&stat.exprs, self);
            if destruct {
                self.space_append_space("]");
            }
        }
    }

    fn label_stat(&mut self, stat: &LabelStat) {
        unimplemented!()
    }

    fn ret_stat(&mut self, stat: &RetStat) {
        self.append_space("return");
        ast_walker::walk_exprlist(&stat.exprs, self);
    }

    fn break_stat(&mut self, _stat: &BreakStat) {
        self.append("break");
    }

    fn goto_stat(&mut self, stat: &GotoStat) {
        unimplemented!()
    }

    fn assign_stat(&mut self, stat: &AssignStat) {
        let destruct = stat.left.len() > 1 && stat.right.len() > 1;
        if destruct {
            self.append_space("[");
        }
        for (n, suffix) in stat.left.iter().enumerate() {
            ast_walker::walk_assinable(suffix, self);
            if n < stat.left.len() - 1 {
                self.append_space(",");
            }
        }
        if destruct {
            self.space_append_space("]");
        }
        self.append_space("=");
        if destruct {
            self.append_space("[");
        }
        ast_walker::walk_exprlist(&stat.right, self);
        if destruct {
            self.space_append_space("]");
        }
    }

    fn call_stat(&mut self, stat: &CallStat) {
        ast_walker::walk_assinable(&stat.call, self);
    }

    fn expr(&mut self, _stat: &Expr) -> bool {
        false
    }

    fn expr_sep(&mut self) {
        self.append(", ");
    }

    fn nil(&mut self) {
        self.append("null");
    }

    fn true_(&mut self) {
        self.append("true");
    }

    fn false_(&mut self) {
        self.append("false");
    }

    fn float(&mut self, f: FloatType) {
        let string = if f.fract() == 0.0 {
            format!("{}.0", f)
        } else {
            f.to_string()
        };
        self.append(&string);
    }

    fn int(&mut self, i: IntType) {
        self.append(&i.to_string());
    }

    fn string(&mut self, s: &str) {
        self.append(s);
    }

    fn vararg(&mut self) {
        self.append("...");
    }

    fn anonymous_func(&mut self) {
        self.append_space("function");
    }

    fn begin_func_body(&mut self, body: &FuncBody) -> bool {
        self.append("(");
        for (n, param) in body.params.iter().enumerate() {
            match param {
                Param::VarArg => self.append("..."),
                Param::Name(s) => self.append(s),
            }
            if n < body.params.len() - 1 {
                self.append(", ");
            }
        }
        self.enter_scope();
        self.append_inc(") {");
        false
    }

    fn end_func_body(&mut self) {
        self.end();
    }

    fn begin_table(&mut self, t: &Table) -> bool {
        if t.fields.len() > 0 {
            self.enter_scope();
            self.append_inc("{");
        } else {
            self.append("{}");
        }

        false
    }
    fn end_table(&mut self, t: &Table) {
        if t.fields.len() > 0 {
            self.leave_scope();
            self.append("}");
        }
    }

    fn field_sep(&mut self) {
        self.append_inc(",");
    }

    fn begin_rec_field(&mut self, _field: &RecField) -> bool {
        false
    }

    fn field_kv_sep(&mut self) {
        self.space_append_space(":");
    }

    fn begin_field_key(&mut self, key: &FieldKey) -> bool {
        match key {
            FieldKey::Expr(_) => self.append_space("["),
            _ => (),
        }
        false
    }

    fn end_field_key(&mut self, key: &FieldKey) {
        match key {
            FieldKey::Expr(_) => self.space_append("]"),
            _ => (),
        }
    }

    fn end_rec_field(&mut self) {}

    fn begin_bin_expr(&mut self, _expr: &BinExpr) -> bool {
        false
    }

    fn binop(&mut self, op: BinOp) {
        let string = match op {
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::Eq => "===",
            BinOp::Ne => "!==",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::BOr => "|",
            BinOp::BXor => "^",
            BinOp::BAnd => "&",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Concat => "+",
            BinOp::Add => "+",
            BinOp::Minus => "-",
            BinOp::Mul => "*",
            BinOp::Mod => "%",
            BinOp::Div => "/",
            BinOp::IDiv => todo!(),
            BinOp::Pow => todo!(),
            _ => unreachable!(),
        };
        self.space_append_space(string);
    }

    fn end_bin_expr(&mut self) {}

    fn begin_un_expr(&mut self, _expr: &UnExpr) -> bool {
        false
    }

    fn unop(&mut self, op: UnOp) {
        match op {
            UnOp::Minus => self.append("-"),
            UnOp::BNot => self.append("~"),
            UnOp::Not => self.append_space("!"),
            UnOp::TLen => todo!(),
            _ => unreachable!(),
        }
    }

    fn end_un_expr(&mut self) {}

    fn begin_suffixed_expr(&mut self, _expr: &SuffixedExpr) -> bool {
        false
    }

    fn end_suffixed_expr(&mut self) {}

    fn name(&mut self, name: &str) {
        self.append(name);
    }

    fn attr(&mut self, attr: &str) {
        self.append(".");
        self.append(attr);
    }

    fn method(&mut self, method: &str) {
        self.append(".");
        self.append(method);
    }

    fn begin_index(&mut self, _expr: &Expr) -> bool {
        self.append("[");
        false
    }

    fn end_index(&mut self) {
        self.append("]");
    }

    fn begin_func_args(&mut self, _args: &FuncArgs) -> bool {
        self.append("(");
        false
    }

    fn end_func_args(&mut self) {
        self.append(")");
    }

    fn begin_paren_expr(&mut self, _expr: &Expr) -> bool {
        self.append("(");
        false
    }

    fn end_paren_expr(&mut self) {
        self.append(")");
    }

    fn suffix(&mut self, _suf: &Suffix) -> bool {
        false
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::File;
    use std::fs::{create_dir, read_dir};
    use std::io::prelude::*;
    use std::path::Path;

    fn try_convert(input: &str) -> String {
        let mut lexer = Lexer::new();
        lexer.set_debug(true);
        lexer.set_use_origin_string(true);
        if let Ok(tokens) = lexer.run(&input) {
            let mut parser = Parser::new();
            parser.set_debug(true);
            if let Ok(ast) = parser.run(tokens) {
                let mut writter = Transpiler::new();
                let output = writter.run(&ast).to_string();
                println!("{}", output);
                return output;
            }
        }
        unreachable!()
    }

    fn convert_lua(src: &str, dst: &str) -> std::io::Result<()> {
        let mut file = File::open(src)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        let output = try_convert(&content);
        let mut file = File::create(dst)?;
        file.write_all(output.as_bytes())?;
        Ok(())
    }

    #[test]
    fn if_else() {
        assert_eq!(
            try_convert("if true then elseif true then else end"),
            "if (true) {\n\
            } else if (true) {\n\
            };\n"
        )
    }

    #[test]
    fn while_block() {
        assert_eq!(
            try_convert("while a > 0 do end"),
            "while (a > 0) {\n\
            };\n"
        )
    }

    #[test]
    fn do_block() {
        assert_eq!(
            try_convert("do end"),
            "{\n};\n"
        )
    }

    #[test]
    fn for_num() {
        assert_eq!(
            try_convert("for i = 1,10,1 do end"),
            "for (let i = 1; i <= 10; i += 1) {\n\
            };\n"
        );
    }

    #[test]
    fn lua_to_js() -> std::io::Result<()> {
        let lua_dir: &'static str = "./lua_tests";
        let tmp: &'static str = "./js_output";
        if let Err(_e) = read_dir(tmp) {
            create_dir(tmp)?
        }

        // convert lua files in `lua` folder to `js` file
        for entry in read_dir(lua_dir)? {
            let entry = entry?;
            let file_name = entry.file_name();
            let name = file_name.to_str().unwrap();
            let jsname = Path::new(&file_name).file_stem().unwrap().to_str().unwrap();
            let src = format!("{}/{}", lua_dir, name);
            let dst = format!("{}/{}", tmp, format!("{}.js", jsname));
            println!("{}, {}", src, dst);
            convert_lua(&src, &dst)?;
        }

        Ok(())
    }
}
