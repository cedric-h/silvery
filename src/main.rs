use quicksilver::{
    geom::Vector,
    graphics::{
        Background::{Col, Img},
        Color, Font, FontStyle,
    },
    lifecycle::{run, Asset, Event, Settings, State, Window},
    Result,
};
mod lang;

pub mod draw {
    use quicksilver::geom::{Rectangle, Transform};
    use quicksilver::graphics::Color;

    #[derive(Debug, Clone, PartialEq)]
    pub enum DrawCommand {
        Rect(Rectangle),
        Color(Color),
        Transform(Transform),
        SaveTransform,
        RestoreTransform,
    }
}

struct QuickTest {
    font: Asset<Font>,
    text: String,
    cursor: usize,
    timer: usize,
    ast: lang::parse::Node,
    ast_dirty: bool,
    eval: lang::Eval,
    eval_error: Option<String>,
    parse_error: Option<String>,
}
impl QuickTest {
    fn cursor_left(&self) -> usize {
        self.cursor.checked_sub(1).unwrap_or(self.text.len() - 1)
    }
    fn cursor_right(&self) -> usize {
        if self.cursor + 1 < self.text.len() - 1 {
            self.cursor + 1
        } else {
            0
        }
    }
    fn type_char(&mut self, c: char) {
        self.text.insert(self.cursor, c);
        self.cursor = self.cursor_right();
    }
}

const START_STRING: &'static str = r#"move 450 250
fill  wave(100)  wave(100) * 0.25 + 0.75  wave(100) * 0.3 + 0.6  1.0

zoom wave(750)*4.5

spin time
move 0 100

spin wave(100)*90 + -45

push
 zoom wave(20) + -1 * 40 + 60
 push
  spin wave(20) * 35 + 90
  rect 0 0 1 wave(20) * 0.25 + 0.75
 pop
 push
  spin wave(20) * -35 + 90
  rect 0 0 -1 wave(20) * -0.25 + -0.75
 pop
pop

fill "red"
rect -12.5 -12.5 25 25

"#;

impl State for QuickTest {
    fn new() -> Result<QuickTest> {
        let mut eval = lang::Eval::new();

        Ok(QuickTest {
            font: Asset::new(Font::load("font.ttf")),
            text: START_STRING.to_string(),
            cursor: START_STRING.len() - 1,
            ast: eval.new_ast(START_STRING).unwrap(),
            ast_dirty: false,
            timer: 0,
            eval: eval,
            eval_error: None,
            parse_error: None,
        })
    }

    fn update(&mut self, _: &mut Window) -> Result<()> {
        self.timer += 1;
        self.eval.set_f32_var("time", self.timer as f32);

        if self.ast_dirty {
            match self.eval.new_ast(&self.text) {
                Ok(ast) => {
                    self.parse_error = None;
                    self.eval_error = self
                        .eval
                        .eval_node(&ast)
                        .err()
                        .map(|e| format!("{}", e));
                    if self.eval_error.is_none() {
                        self.ast = ast;
                    }
                }
                Err(err) => self.parse_error = Some(format!("{}", err)),
            };
            self.ast_dirty = false;
        }

        Ok(())
    }

    fn event(&mut self, event: &Event, _: &mut Window) -> Result<()> {
        use quicksilver::input::{
            ButtonState::Pressed,
            Key::{Back, Left, Return, Right},
        };
        match event {
            &Event::Typed(c) => {
                self.ast_dirty = true;
                self.type_char(c);
            }
            &Event::Key(k, Pressed) => {
                self.ast_dirty = true;
                match k {
                    Back => {
                        if self.text.len() > 0 {
                            self.cursor = self.cursor_left();
                            self.text.remove(self.cursor);
                            if self.cursor == self.text.len() {
                                self.cursor = self.cursor_left();
                            }
                        }
                    }
                    Return => self.type_char('\n'),
                    Left => self.cursor = self.cursor_left(),
                    Right => self.cursor = self.cursor_right(),
                    _ => {},
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn draw(&mut self, window: &mut Window) -> Result<()> {
        // Remove any lingering artifacts from the previous frame
        window.clear(Color::BLACK)?;

        let mut color = Color::WHITE;
        let mut transforms = vec![quicksilver::geom::Transform::IDENTITY];

        self.eval.draws.drain(..);
        match self.eval.eval_node(&self.ast) {
            Ok(_) => {}
            Err(e) => {
                // this really shouldn't happen, we should save asts that don't error.
                println!("bad ast slipped in!");
                self.eval_error = Some(format!("{}", e));
            }
        }
        for cmd in self.eval.draws.iter() {
            use draw::DrawCommand::*;
            match cmd {
                Rect(rect) => window.draw_ex(rect, Col(color), *transforms.last().unwrap(), 0),
                Color(new_color) => color = *new_color,
                Transform(t) => {
                    let now = transforms.last_mut().unwrap();
                    *now = *now * *t;
                }
                SaveTransform => {
                    let now = transforms.last().unwrap().clone();
                    transforms.push(now)
                }
                RestoreTransform => {
                    if transforms.len() >= 2 {
                        transforms.pop();
                    }
                }
            }
        }

        let mut text = self.text.clone();
        if self.timer % 40 < 25 {
            let i = self.cursor;

            let cursor_text = if text.chars().nth(i).unwrap() == '\n' {
                "_\n"
            } else {
                "_"
            };

            text.replace_range(i..=i, cursor_text);
        };

        let font = &mut self.font;
        let error = self.parse_error.as_ref().or(self.eval_error.as_ref());

        font.execute(|font| {
            let img = font.render(text.as_str(), &FontStyle::new(24.0, Color::WHITE))?;
            window.draw(&img.area(), Img(&img));

            if let Some(error) = error {
                use quicksilver::geom::Shape;
                let img = font.render(error.as_str(), &FontStyle::new(24.0, Color::RED))?;
                window.draw(&img.area().translate((300, 0)), Img(&img));
            }

            Ok(())
        })?;

        Ok(())
    }
}

fn main() {
    run::<QuickTest>(
        "QuickTest",
        Vector::new(800, 600),
        Settings {
            multisampling: Some(16),
            scale: quicksilver::graphics::ImageScaleStrategy::Blur,
            ..Settings::default()
        },
    );
}
