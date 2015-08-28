use unjson::ty::Object;
use unjson::{ExtractField, Unjson};
use easter::id::Id;
use easter::expr::Expr;
use easter::stmt::{Stmt, StmtListItem, Case, Catch};
use easter::patt::{Patt, AssignTarget};
use easter::obj::Prop;
use easter::decl::Dtor;
use easter::cover::IntoAssignPatt;

use error::Error;
use result::{Result, Map};
use id::IntoId;
use stmt::IntoStmt;
use expr::IntoExpr;
use patt::IntoPatt;
use obj::IntoObj;
use decl::IntoDecl;

pub trait ExtractNode {
    fn extract_id(&mut self, &'static str) -> Result<Id>;
    fn extract_id_opt(&mut self, &'static str) -> Result<Option<Id>>;
    fn extract_assign_patt(&mut self, &'static str) -> Result<Patt<AssignTarget>>;
    fn extract_stmt(&mut self, &'static str) -> Result<Stmt>;
    fn extract_expr(&mut self, &'static str) -> Result<Expr>;
    fn extract_expr_opt(&mut self, &'static str) -> Result<Option<Expr>>;
    fn extract_expr_list(&mut self, &'static str) -> Result<Vec<Expr>>;
    fn extract_expr_opt_list(&mut self, &'static str) -> Result<Vec<Option<Expr>>>;
    fn extract_stmt_opt(&mut self, &'static str) -> Result<Option<Stmt>>;
    fn extract_stmt_list(&mut self, &'static str) -> Result<Vec<StmtListItem>>;
    fn extract_patt(&mut self, &'static str) -> Result<Patt<Id>>;
    fn extract_patt_list(&mut self, &'static str) -> Result<Vec<Patt<Id>>>;
    fn extract_prop_list(&mut self, &'static str) -> Result<Vec<Prop>>;
    fn extract_dtor_list(&mut self, &'static str) -> Result<Vec<Dtor>>;
    fn extract_case_list(&mut self, &'static str) -> Result<Vec<Case>>;
    fn extract_catch_opt(&mut self, &'static str) -> Result<Option<Catch>>;
}

impl ExtractNode for Object {
    fn extract_id(&mut self, name: &'static str) -> Result<Id> {
        self.extract_object(name).map_err(Error::Json).and_then(|o| o.into_id())
    }

    fn extract_id_opt(&mut self, name: &'static str) -> Result<Option<Id>> {
        Ok(match try!(self.extract_object_opt(name).map_err(Error::Json)) {
            Some(obj) => Some(try!(obj.into_id())),
            None      => None
        })
    }

    fn extract_assign_patt(&mut self, name: &'static str) -> Result<Patt<AssignTarget>> {
        let expr = try!(self.extract_expr(name));
        match expr.into_assign_patt() {
            Ok(patt) => Ok(patt),
            _ => Err(Error::InvalidLHS(name))
        }
    }

    fn extract_stmt(&mut self, name: &'static str) -> Result<Stmt> {
        self.extract_object(name).map_err(Error::Json).and_then(|o| o.into_stmt())
    }

    fn extract_expr(&mut self, name: &'static str) -> Result<Expr> {
        self.extract_object(name).map_err(Error::Json).and_then(|o| o.into_expr())
    }

    fn extract_expr_opt(&mut self, name: &'static str) -> Result<Option<Expr>> {
        Ok(match try!(self.extract_object_opt(name).map_err(Error::Json)) {
            Some(o) => Some(try!(o.into_expr())),
            None => None
        })
    }

    fn extract_expr_list(&mut self, name: &'static str) -> Result<Vec<Expr>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_expr())
    }

    fn extract_expr_opt_list(&mut self, name: &'static str) -> Result<Vec<Option<Expr>>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        list.map(|v| {
            match try!(v.into_object_opt().map_err(Error::Json)) {
                None => Ok(None),
                Some(o) => o.into_expr().map(Some)
            }
        })
    }

    fn extract_stmt_opt(&mut self, name: &'static str) -> Result<Option<Stmt>> {
        Ok(match try!(self.extract_object_opt(name).map_err(Error::Json)) {
            Some(o) => Some(try!(o.into_stmt())),
            None => None
        })
    }

    fn extract_stmt_list(&mut self, name: &'static str) -> Result<Vec<StmtListItem>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_stmt_list_item())
    }

    fn extract_patt(&mut self, name: &'static str) -> Result<Patt<Id>> {
        self.extract_object(name).map_err(Error::Json).and_then(|o| o.into_patt())
    }

    fn extract_patt_list(&mut self, name: &'static str) -> Result<Vec<Patt<Id>>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_patt())
    }

    fn extract_prop_list(&mut self, name: &'static str) -> Result<Vec<Prop>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_prop())
    }

    fn extract_dtor_list(&mut self, name: &'static str) -> Result<Vec<Dtor>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_dtor())
    }

    fn extract_case_list(&mut self, name: &'static str) -> Result<Vec<Case>> {
        let list = try!(self.extract_array(name).map_err(Error::Json));
        let objs = try!(list.map(|v| v.into_object().map_err(Error::Json)));
        objs.map(|o| o.into_case())
    }

    fn extract_catch_opt(&mut self, name: &'static str) -> Result<Option<Catch>> {
        Ok(match try!(self.extract_object_opt(name).map_err(Error::Json)) {
            Some(o) => Some(try!(o.into_catch())),
            None => None
        })
    }

}
