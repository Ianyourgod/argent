#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub table: HashMap<String, Type>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Type) {
        self.table.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Type> {
        self.table.get(key)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Fn(Vec<Type>, Box<Type>),
    Identifier(String),
}