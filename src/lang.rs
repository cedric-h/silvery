pub use eval::Eval;
pub use lex::lex;
pub use parse::parse;

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Float(f32),
    Text(String),
}
impl Value {}
impl std::convert::TryInto<f32> for &Value {
    type Error = eval::Error;
    fn try_into(self) -> Result<f32, Self::Error> {
        match self {
            Value::Float(f) => Ok(*f),
            _ => Err(eval::Error::DifferentType(self.clone())),
        }
    }
}
impl std::convert::TryInto<f32> for Value {
    type Error = eval::Error;
    fn try_into(self) -> Result<f32, Self::Error> {
        (&self).try_into()
    }
}
impl<'a> std::convert::TryInto<&'a str> for &'a Value {
    type Error = eval::Error;
    fn try_into(self) -> Result<&'a str, Self::Error> {
        match self {
            Value::Text(s) => Ok(s),
            _ => Err(eval::Error::DifferentType(self.clone())),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum BinOp {
    Mod,
    Div,
    Mul,
    Add,
    Sub,
}
impl BinOp {
    fn from_char(c: char) -> Option<Self> {
        use BinOp::*;

        match c {
            '%' => Some(Mod),
            '/' => Some(Div),
            '*' => Some(Mul),
            '+' => Some(Add),
            '-' => Some(Sub),
            _ => None,
        }
    }

    fn eval(&self, left: f32, right: f32) -> f32 {
        use BinOp::*;

        match *self {
            Mod => left % right,
            Div => left / right,
            Mul => left * right,
            Add => left + right,
            Sub => left - right,
        }
    }
}

mod lex {
    #[derive(PartialEq, Debug, Clone)]
    pub enum Lexeme {
        Variable(super::NameInternIndex),
        Literal(super::Value),
        BinOp(super::BinOp),
        FunctionCallStart,
        FunctionCallStop,
    }
    impl Lexeme {
        pub fn into_variable(self) -> Option<super::NameInternIndex> {
            match self {
                Lexeme::Variable(i) => Some(i),
                _ => None,
            }
        }
        pub fn is_function_call_stop(&self) -> bool {
            match self {
                Lexeme::FunctionCallStop => true,
                _ => false,
            }
        }
    }

    pub type LexemeLine = Vec<Lexeme>;

    pub fn lex(input: &str, namer: &mut super::NameInterner) -> Vec<LexemeLine> {
        fn lex_word(lexemes: &mut Vec<Lexeme>, namer: &mut super::NameInterner, word: &str) {
            if word.is_empty() {
                return;
            }

            for (i, c) in word.chars().enumerate() {
                if let Some(op) = super::BinOp::from_char(c) {
                    let mut split = word.splitn(2, c);

                    if op == super::BinOp::Sub && i == 0 {
                        continue;
                    }

                    if let Some(word) = split.next() {
                        lex_word(lexemes, namer, word);
                    }

                    lexemes.push(Lexeme::BinOp(op));

                    split.for_each(|word| lex_word(lexemes, namer, word));

                    return;
                }
                if '(' == c || ')' == c {
                    let mut split = word.splitn(2, c);

                    if let Some(word) = split.next() {
                        lex_word(lexemes, namer, word);
                    }

                    lexemes.push(match c {
                        '(' => Lexeme::FunctionCallStart,
                        ')' => Lexeme::FunctionCallStop,
                        _ => unreachable!(),
                    });

                    split.for_each(|word| lex_word(lexemes, namer, word));

                    return;
                }
            }

            if let Ok(num) = word.parse::<f32>() {
                lexemes.push(Lexeme::Literal(super::Value::Float(num)));
            } else if let Some('"') = word.chars().next() {
                lexemes.push(Lexeme::Literal(super::Value::Text(
                    word.trim_matches('"').to_string(),
                )));
            } else {
                lexemes.push(Lexeme::Variable(namer.intern(word)));
            }
        }

        input
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                let mut lexemes = Vec::with_capacity(line.len());
                line.split(' ').fold(&mut lexemes, |lexemes, word| {
                    lex_word(lexemes, namer, word);
                    lexemes
                });
                lexemes
            })
            .collect()
    }

    #[test]
    fn unit() {
        use Lexeme::*;

        fn float(f: f32) -> Lexeme {
            Literal(super::Value::Float(f))
        }

        let mut interner = super::NameInterner::new();
        let wave = interner.intern("wave");
        let rect = interner.intern("rect");

        assert_eq!(
            vec![vec![
                Variable(rect),
                float(10.0),
                float(10.0),
                float(25.0),
                float(25.0),
            ]],
            lex("rect 10 10 25 25", &mut interner)
        );

        assert_eq!(
            vec![vec![
                Variable(rect),
                float(10.0),
                float(10.0),
                Variable(rect + 1),
                Variable(rect + 1),
            ]],
            lex("rect 10 10 time time", &mut interner)
        );

        assert_eq!(
            vec![vec![float(3.0), BinOp(super::BinOp::Mod), float(2.0),]],
            lex("3 % 2", &mut interner)
        );

        assert_eq!(
            vec![vec![float(342.0), BinOp(super::BinOp::Mod), float(2484.0),]],
            lex("342%2484", &mut interner)
        );

        assert_eq!(
            vec![vec![
                Variable(wave),
                FunctionCallStart,
                float(1000.0),
                FunctionCallStop,
            ]],
            lex("wave(1000)", &mut interner)
        );

        assert_eq!(
            vec![vec![
                Variable(wave),
                FunctionCallStart,
                Variable(wave),
                FunctionCallStart,
                float(1000.0),
                FunctionCallStop,
                FunctionCallStop,
            ]],
            lex("wave(wave(1000))", &mut interner)
        );
    }
}

pub mod parse {
    use std::fmt;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Error {
        NoLeftForBinOp,
        UnexpectedEnd,
        FunctionBeforeParen,
        NoCommandStart,
        FunctionNeverClosed,
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Error::NoLeftForBinOp => write!(
                    f,
                    "Invalid or nonexistent value to the left of binary operator!"
                ),
                Error::UnexpectedEnd => write!(f, "Unexpected end of input!"),
                Error::FunctionBeforeParen => {
                    write!(f, "A valid function name must proceed \"(\"!")
                }
                Error::NoCommandStart => write!(f, "A valid command must start each line!"),
                Error::FunctionNeverClosed => write!(f, "A function is started but never ended!"),
            }
        }
    }
    impl std::error::Error for Error {}
    type Result<T> = std::result::Result<T, Error>;

    /// Makes up the AST
    #[derive(Clone, Debug, PartialEq)]
    pub enum Node {
        Block(Vec<Node>),
        CallCommand {
            function: super::NameInternIndex,
            args: Vec<Node>,
        },
        CallFunction {
            function: super::NameInternIndex,
            args: Vec<Node>,
        },
        Variable(super::NameInternIndex),
        Value(super::Value),
        BinOp {
            left: Box<Node>,
            op: super::BinOp,
            right: Box<Node>,
        },
    }
    impl Node {
        fn var(&self) -> Option<super::NameInternIndex> {
            match self {
                Node::Variable(i) => Some(*i),
                _ => None,
            }
        }
    }

    fn parse_lexeme_cluster(
        lexemes: &mut Vec<super::lex::Lexeme>,
        stack: &mut Vec<Node>,
    ) -> Result<()> {
        use super::lex::Lexeme;

        match lexemes.pop().ok_or(Error::UnexpectedEnd)? {
            Lexeme::Variable(i) => stack.push(Node::Variable(i)),
            Lexeme::Literal(val) => stack.push(Node::Value(val)),
            Lexeme::BinOp(op) => {
                let left = Box::new(stack.pop().ok_or(Error::NoLeftForBinOp)?);

                // get right
                parse_lexeme_cluster(lexemes, stack)?;
                let right = Box::new(stack.pop().ok_or(Error::UnexpectedEnd)?);

                stack.push(Node::BinOp { left, op, right });
            }
            Lexeme::FunctionCallStart => {
                let function = stack
                    .pop()
                    .and_then(|x| x.var())
                    .ok_or(Error::FunctionBeforeParen)?;

                let call_end = lexemes
                    .iter()
                    .rposition(|lexeme| lexeme.is_function_call_stop())
                    .ok_or(Error::FunctionNeverClosed)?;
                let mut args_lexemes = lexemes.split_off(call_end);

                let mut args = Vec::with_capacity(args_lexemes.len());
                while args_lexemes.last().is_some() {
                    parse_lexeme_cluster(&mut args_lexemes, &mut args)?;
                }

                stack.push(Node::CallFunction { function, args });
            }
            Lexeme::FunctionCallStop => {} //lexeme => panic!("Can't parse lexeme: {:?}", lexeme),
        }

        Ok(())
    }

    pub fn parse(lines: Vec<super::lex::LexemeLine>, _: &mut super::NameInterner) -> Result<Node> {
        Ok(Node::Block(
            lines
                .into_iter()
                .map(|mut words| {
                    words.reverse();
                    Ok(Node::CallCommand {
                        function: words
                            .pop()
                            .and_then(|x| x.into_variable())
                            .ok_or(Error::NoCommandStart)?,
                        args: {
                            let mut nodes = Vec::with_capacity(words.len());
                            while !words.is_empty() {
                                parse_lexeme_cluster(&mut words, &mut nodes)?;
                            }
                            nodes
                        },
                    })
                })
                .collect::<Result<Vec<_>>>()?,
        ))
    }

    #[test]
    fn unit() {
        use super::Value::Float;
        use Node::*;

        let mut namer = super::NameInterner::new();
        let rect = namer.intern("rect");
        let wave = namer.intern("wave");

        assert_eq!(
            Ok(Block(vec![CallCommand {
                function: rect,
                args: (1..5).map(|x| Value(Float(x as f32))).collect()
            }])),
            parse(
                {
                    use super::lex::Lexeme;
                    vec![vec![
                        Lexeme::Variable(rect),
                        Lexeme::Literal(Float(1.0)),
                        Lexeme::Literal(Float(2.0)),
                        Lexeme::Literal(Float(3.0)),
                        Lexeme::Literal(Float(4.0)),
                    ]]
                },
                &mut namer
            )
        );

        let mut lexemes = {
            use super::lex::Lexeme;
            vec![
                Lexeme::Variable(wave),
                Lexeme::FunctionCallStart,
                Lexeme::Literal(Float(1000.0)),
                Lexeme::FunctionCallStop,
            ]
        };
        lexemes.reverse();
        let mut nodes = Vec::with_capacity(lexemes.len());
        while lexemes.last().is_some() {
            parse_lexeme_cluster(&mut lexemes, &mut nodes).unwrap()
        }

        assert_eq!(
            vec![CallFunction {
                function: wave,
                args: vec![Value(Float(1000.0))]
            }],
            nodes
        );
    }

    #[test]
    fn integration() {
        use super::Value::Float;
        use Node::*;

        let mut namer = super::NameInterner::new();
        let rect = namer.intern("rect");
        let wave = namer.intern("wave");

        assert_eq!(
            Ok(Block(vec![CallCommand {
                function: rect,
                args: vec![
                    Value(Float(10.0)),
                    Value(Float(10.0)),
                    CallFunction {
                        function: wave,
                        args: vec![Value(Float(1000.0))],
                    },
                    Value(Float(100.0)),
                ]
            },])),
            parse(
                super::lex::lex("rect 10 10 wave(1000) 100", &mut namer),
                &mut namer
            )
        );

        assert_eq!(
            Ok(Block(vec![CallCommand {
                function: rect,
                args: vec![
                    Value(Float(10.0)),
                    Value(Float(10.0)),
                    CallFunction {
                        function: wave,
                        args: vec![CallFunction {
                            function: wave,
                            args: vec![Value(Float(1000.0))],
                        },],
                    },
                    Value(Float(100.0)),
                ]
            },])),
            parse(
                super::lex::lex("rect 10 10 wave(wave(1000)) 100", &mut namer),
                &mut namer
            )
        );
    }
}

pub mod eval {
    use super::parse::Node;
    use std::convert::TryInto;
    use std::fmt;
    use std::rc::Rc;

    #[derive(Debug)]
    pub enum Error {
        UnknownColor(String),
        ParseError(super::parse::Error),
        NotEnoughArgs { needed: usize, got: usize },
        UnknownCommand(super::NameInternIndex),
        UnknownFunction(super::NameInternIndex),
        UnknownVariable(super::NameInternIndex),
        DifferentType(super::Value),
        NotValue(Node),
    }
    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Error::UnknownColor(color) => write!(f, "Unknown color: {:?}", color),
                Error::NotEnoughArgs { needed, got } => {
                    write!(f, "Function needed {} args, got {}", needed, got)
                }
                Error::UnknownCommand(i) => write!(f, "Unknown command: {}", i),
                Error::UnknownFunction(i) => write!(f, "Unknown function: {}", i),
                Error::UnknownVariable(i) => write!(f, "Unknown variable: {}", i),
                Error::ParseError(e) => write!(f, "parsing error: {}", e),
                Error::NotValue(node) => write!(f, "Not a value: {:?}", node),
                Error::DifferentType(val) => write!(f, "{:?} doesn't contain that type!", val),
            }
        }
    }
    impl std::error::Error for Error {}

    type Result<T> = std::result::Result<T, Error>;

    #[derive(Default)]
    struct Values(fxhash::FxHashMap<super::NameInternIndex, super::Value>);
    impl Values {}

    #[derive(Debug)]
    struct Args(Vec<Node>);
    impl Args {
        //pub fn one<'a>(&'a self, eval: &'a Eval) -> Result<&'a super::Value> {
        pub fn one(&self, eval: &Eval) -> Result<super::Value> {
            let first = self
                .0
                .last()
                .ok_or_else(|| Error::NotEnoughArgs { needed: 1, got: 0 })?;
            eval.get(&first)
        }
        //pub fn list<'a>(&'a self, eval: &'a Eval, len: usize) -> Result<Vec<&'a super::Value>> {
        pub fn list(&self, eval: &Eval, len: usize) -> Result<Vec<super::Value>> {
            let mut args: Vec<super::Value> = self
                .0
                .iter()
                .map(|node| eval.get(node))
                .collect::<Result<Vec<super::Value>>>()?;
            (0..len)
                .map(|i| {
                    args.pop().ok_or_else(|| Error::NotEnoughArgs {
                        needed: len,
                        got: i,
                    })
                })
                .collect()
        }
        pub fn f32_list(&self, eval: &Eval, len: usize) -> Result<Vec<f32>> {
            self.list(eval, len)?
                .into_iter()
                .map(|x| x.try_into())
                .collect::<Result<Vec<f32>>>()
        }
    }

    type Commands =
        fxhash::FxHashMap<super::NameInternIndex, Box<dyn Fn(&mut Eval, Args) -> Result<()>>>;

    type Functions =
        fxhash::FxHashMap<super::NameInternIndex, Box<dyn Fn(&Eval, Args) -> Result<super::Value>>>;

    #[derive(Default)]
    pub struct Eval {
        pub draws: Vec<crate::draw::DrawCommand>,
        values: Values,
        commands: Rc<Commands>,
        functions: Functions,
        namer: super::NameInterner,
    }

    impl Eval {
        //fn get<'a>(&'a self, node: &'a Node) -> Result<&'a super::Value> {
        fn get(&self, node: &Node) -> Result<super::Value> {
            match node {
                Node::Variable(i) => self
                    .values
                    .0
                    .get(i)
                    .cloned()
                    .ok_or_else(|| Error::UnknownVariable(*i)),
                Node::Value(v) => Ok(v.clone()),
                Node::BinOp { left, op, right } => {
                    let l: f32 = self.get(left)?.try_into()?;
                    let r: f32 = self.get(right)?.try_into()?;
                    Ok(super::Value::Float(op.eval(l, r)))
                }
                Node::CallFunction { function, args } => {
                    self.call_function(*function, args.to_vec())
                }
                _ => Err(Error::NotValue(node.clone())),
            }
        }

        pub fn new() -> Self {
            use fxhash::FxHashMap;
            use quicksilver::graphics::Color;

            let mut namer = super::NameInterner::default();

            let mut functions = Functions::default();
            functions.insert(
                namer.intern("wave"),
                Box::new(|eval: &Eval, args: Args| {
                    let milliseconds: f32 = args.one(eval)?.try_into()?;
                    let time_raw = eval.get_f32_var("time")?;
                    let time = time_raw % (milliseconds * 2.0);

                    Ok(super::Value::Float(
                        if time < milliseconds {
                            time
                        } else {
                            milliseconds - (time - milliseconds)
                        } / milliseconds,
                    ))
                }),
            );

            let mut commands = Commands::default();

            const COLORS: &'static [(&'static str, Color)] = &[
                ("white", Color::WHITE),
                ("black", Color::BLACK),
                ("red", Color::RED),
                ("orange", Color::ORANGE),
                ("yellow", Color::YELLOW),
                ("green", Color::GREEN),
                ("cyan", Color::CYAN),
                ("blue", Color::BLUE),
                ("magenta", Color::MAGENTA),
                ("purple", Color::PURPLE),
                ("indigo", Color::INDIGO),
            ];
            let colors: FxHashMap<_, _> = COLORS.iter().copied().collect();
            commands.insert(
                namer.intern("fill"),
                Box::new(move |eval: &mut Eval, args: Args| {
                    let requested_color = if args.0.len() == 1 {
                        let arg = args.one(eval)?;
                        let color_name: &str = (&arg).try_into()?;
                        *colors
                            .get(color_name)
                            .ok_or_else(|| Error::UnknownColor(color_name.to_string()))?
                    } else {
                        let mut args = args.f32_list(eval, 4)?;
                        args.reverse();
                        quicksilver::graphics::Color {
                            r: args[0],
                            g: args[1],
                            b: args[2],
                            a: args[3],
                        }
                    };
                    eval.draws
                        .push(crate::draw::DrawCommand::Color(requested_color));

                    Ok(())
                }),
            );

            commands.insert(
                namer.intern("rect"),
                Box::new(|eval: &mut Eval, args: Args| {
                    use quicksilver::geom::{Rectangle, Transform, Vector};

                    let mut nums = args.f32_list(eval, 4)?;
                    let pos = Vector::new(nums.pop().unwrap(), nums.pop().unwrap());
                    let size = Vector::new(nums.pop().unwrap(), nums.pop().unwrap());

                    eval.draws.push(crate::draw::DrawCommand::SaveTransform);
                    eval.draws
                        .push(crate::draw::DrawCommand::Transform(Transform::translate(
                            pos + size / 2.0,
                        )));
                    eval.draws
                        .push(crate::draw::DrawCommand::Rect(Rectangle::new(
                            -size / 2.0,
                            size,
                        )));
                    eval.draws.push(crate::draw::DrawCommand::RestoreTransform);

                    Ok(())
                }),
            );

            commands.insert(
                namer.intern("push"),
                Box::new(move |eval: &mut Eval, _: Args| {
                    Ok(eval.draws.push(crate::draw::DrawCommand::SaveTransform))
                }),
            );
            commands.insert(
                namer.intern("pop"),
                Box::new(move |eval: &mut Eval, _: Args| {
                    Ok(eval.draws.push(crate::draw::DrawCommand::RestoreTransform))
                }),
            );

            commands.insert(
                namer.intern("move"),
                Box::new(|eval: &mut Eval, args: Args| {
                    let mut nums = args.f32_list(eval, 2)?;
                    eval.draws.push(crate::draw::DrawCommand::Transform(
                        quicksilver::geom::Transform::translate((
                            nums.pop().unwrap(),
                            nums.pop().unwrap(),
                        )),
                    ));

                    Ok(())
                }),
            );

            commands.insert(
                namer.intern("spin"),
                Box::new(move |eval: &mut Eval, args: Args| {
                    let angle: f32 = args.one(eval)?.try_into()?;
                    eval.draws.push(crate::draw::DrawCommand::Transform(
                        quicksilver::geom::Transform::rotate(angle),
                    ));

                    Ok(())
                }),
            );

            commands.insert(
                namer.intern("zoom"),
                Box::new(move |eval: &mut Eval, args: Args| {
                    let nums = args.f32_list(eval, 2).or_else(|_| args.f32_list(eval, 1))?;
                    eval.draws
                        .push(crate::draw::DrawCommand::Transform(match nums.len() {
                            0 => panic!("Args::f32_list returned empty list, input: {:?}", args),
                            1 => quicksilver::geom::Transform::scale((nums[0], nums[0])),
                            _ => quicksilver::geom::Transform::scale((nums[0], nums[1])),
                        }));

                    Ok(())
                }),
            );

            Self {
                commands: Rc::new(commands),
                functions,
                namer,
                ..Default::default()
            }
        }

        pub fn set_f32_var(&mut self, var_name: &str, value: f32) {
            let namer = &mut self.namer;
            let values = &mut self.values.0;
            values.insert(namer.intern(var_name), super::Value::Float(value));
        }

        pub fn get_f32_var(&self, var_name: &str) -> Result<f32> {
            let val = self
                .namer
                .names
                .get(var_name)
                .and_then(|i| self.values.0.get(&i))
                .ok_or_else(|| Error::UnknownVariable(0))?;
            match val {
                super::Value::Float(f) => Ok(*f),
                _ => Err(Error::DifferentType(val.clone())),
            }
        }

        fn call_command(&mut self, i: super::NameInternIndex, args: Vec<Node>) -> Result<()> {
            let commands = Rc::clone(&self.commands);
            commands.get(&i).ok_or_else(|| Error::UnknownCommand(i))?(self, Args(args))
        }

        fn call_function(
            &self,
            i: super::NameInternIndex,
            args: Vec<Node>,
        ) -> Result<super::Value> {
            self.functions
                .get(&i)
                .ok_or_else(|| Error::UnknownFunction(i))?(self, Args(args))
        }

        pub fn new_ast(&mut self, text: &str) -> Result<Node> {
            self.draws.clear();

            let lexemes = super::lex(&text, &mut self.namer);
            super::parse(lexemes, &mut self.namer).map_err(|e| Error::ParseError(e))
        }

        pub fn eval_node(&mut self, node: &Node) -> Result<()> {
            match node {
                Node::Block(ast) => ast
                    .into_iter()
                    .map(|n| self.eval_node(n))
                    .collect::<Result<()>>()?,
                Node::CallCommand { function, args } => {
                    self.call_command(*function, args.to_vec())?;
                }
                _ => unreachable!(),
            };

            Ok(())
        }
    }

    #[test]
    fn integration() {
        use crate::draw::DrawCommand;
        use quicksilver::geom::{Rectangle, Transform};

        let mut eval = Eval::new();

        eval.eval("rect 5 10 10 20").unwrap();

        assert_eq!(
            vec![
                DrawCommand::SaveTransform,
                DrawCommand::Transform(Transform::translate((5.0 + 5.0, 10.0 + 10.0))),
                DrawCommand::Rect(Rectangle::new((-5.0, -10.0), (10.0, 20.0))),
                DrawCommand::RestoreTransform,
            ],
            eval.draws
        );
    }

    #[test]
    fn unit() {
        use crate::draw::DrawCommand;
        use quicksilver::geom::Transform;

        let mut eval = Eval::new();

        let spin = eval.namer.intern("spin");

        eval.eval_node(Node::Block(vec![Node::CallCommand {
            function: spin,
            args: vec![Node::BinOp {
                left: Box::new(Node::Value(super::Value::Float(21.0))),
                op: super::BinOp::Mod,
                right: Box::new(Node::Value(super::Value::Float(5.0))),
            }],
        }]))
        .unwrap();

        assert_eq!(
            vec![DrawCommand::Transform(Transform::rotate(1))],
            eval.draws
        );
    }
}

type NameInternIndex = usize;
#[derive(Default)]
pub struct NameInterner {
    names: fxhash::FxHashMap<String, NameInternIndex>,
}
impl NameInterner {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Default::default()
    }

    pub fn intern(&mut self, name: &str) -> NameInternIndex {
        // can't use the Entry API here because then we'd have to copy the string
        if let Some(index) = self.names.get(name) {
            *index
        } else {
            let len = self.names.len();
            self.names.insert(name.to_string(), len);
            len
        }
    }
}
