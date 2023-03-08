use crate::{Part, Solution};

enum Shape {
    Rock,
    Paper,
    Scissors,
}
enum Outcome {
    Lose,
    Draw,
    Win,
}

impl TryInto<Shape> for char {
    type Error = char;

    fn try_into(self) -> Result<Shape, Self::Error> {
        match self {
            'A' => Ok(Shape::Rock),
            'B' => Ok(Shape::Paper),
            'C' => Ok(Shape::Scissors),
            'X' => Ok(Shape::Rock),
            'Y' => Ok(Shape::Paper),
            'Z' => Ok(Shape::Scissors),
            c => Err(c),
        }
    }
}
impl TryInto<Outcome> for char {
    type Error = char;

    fn try_into(self) -> Result<Outcome, Self::Error> {
        match self {
            'X' => Ok(Outcome::Lose),
            'Y' => Ok(Outcome::Draw),
            'Z' => Ok(Outcome::Win),
            c => Err(c),
        }
    }
}
trait Scored {
    fn score(thing: &Self) -> usize;
}

impl Scored for Shape {
    fn score(s: &Shape) -> usize {
        match s {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }
}
impl Scored for Outcome {
    fn score(o: &Outcome) -> usize {
        match o {
            Outcome::Lose => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }
}
/// solve problem for day 2
pub fn day2(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let my_move: Shape = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let outcome = match (&my_move, &opponent_move) {
                (Shape::Rock, Shape::Rock) => Outcome::Draw,
                (Shape::Paper, Shape::Paper) => Outcome::Draw,
                (Shape::Scissors, Shape::Scissors) => Outcome::Draw,
                (Shape::Rock, Shape::Paper) => Outcome::Lose,
                (Shape::Paper, Shape::Rock) => Outcome::Win,
                (Shape::Scissors, Shape::Paper) => Outcome::Win,
                (Shape::Paper, Shape::Scissors) => Outcome::Lose,
                (Shape::Rock, Shape::Scissors) => Outcome::Win,
                (Shape::Scissors, Shape::Rock) => Outcome::Lose,
            };
            accu + Scored::score(&outcome) + Scored::score(&my_move)
        })
    }
    fn part2(input: &str) -> usize {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let desired_outcome: Outcome = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let my_move = match (&opponent_move, &desired_outcome) {
                (Shape::Rock, Outcome::Lose) => Shape::Scissors,
                (Shape::Rock, Outcome::Draw) => Shape::Rock,
                (Shape::Rock, Outcome::Win) => Shape::Paper,
                (Shape::Paper, Outcome::Lose) => Shape::Rock,
                (Shape::Paper, Outcome::Draw) => Shape::Paper,
                (Shape::Paper, Outcome::Win) => Shape::Scissors,
                (Shape::Scissors, Outcome::Lose) => Shape::Paper,
                (Shape::Scissors, Outcome::Draw) => Shape::Scissors,
                (Shape::Scissors, Outcome::Win) => Shape::Rock,
            };
            accu + Scored::score(&desired_outcome) + Scored::score(&my_move)
        })
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
