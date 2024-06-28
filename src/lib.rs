// Big ups to Glickman, http://www.glicko.net/glicko/glicko2.pdf

use std::{
    f64::consts::PI,
    fmt::Debug,
    ops::{Add, Div, Mul, Neg, Sub},
};

#[derive(Debug, Clone, Copy)]
pub struct Glicko2 {
    rating: f64,
    deviation: f64,
    volatility: f64,
}

pub trait Score: Debug {
    // Should be a number between 0 and 1
    // Usually: 1 == Win, 0 == Lost, 0.5 == Draw.
    fn score(&self) -> f64;
}
impl Score for f64 {
    fn score(&self) -> f64 {
        *self
    }
}

impl Glicko2 {
    pub fn new(rating: f64, deviation: f64, volatility: f64) -> Self {
        Self {
            rating,
            deviation,
            volatility,
        }
    }

    fn g_factor(deviation: f64) -> f64 {
        1_f64 / deviation.div(PI).powi(2).mul(3_f64).add(1_f64).sqrt()
    }

    fn estimated_outcome(rating: f64, op_rating: f64, op_g_factor: f64) -> f64 {
        1_f64 / op_g_factor.mul(rating - op_rating).neg().exp().add(1_f64)
    }

    fn estimated_variance(
        ops_g_factor_squared: &[f64],
        estimated_outcome: &[f64],
    ) -> f64 {
        1_f64
            / ops_g_factor_squared
                .iter()
                .zip(estimated_outcome)
                .map(|(&g2, &e)| g2 * e * 1_f64.sub(e))
                .sum::<f64>()
    }

    fn estimated_improvement(
        estimated_variance: f64,
        ops_g_factor: &[f64],
        estimated_outcome: &[f64],
        scores: &[&impl Score],
    ) -> f64 {
        estimated_variance
            * ops_g_factor
                .iter()
                .zip(estimated_outcome)
                .zip(scores)
                .map(|((g, e), s)| g * s.score().sub(e))
                .sum::<f64>()
    }

    fn update_volatility(
        &mut self,
        deviation_squared: f64,
        estimated_variance: f64,
        estimated_improvement: f64,
        accuracy: f64,
        constraint: f64,
    ) {
        let improvement2 = estimated_improvement.powi(2);
        let constants = [
            self.volatility.powi(2).ln(),
            constraint.powi(2),
            improvement2 - deviation_squared,
            deviation_squared + estimated_variance,
        ];

        let f = |x: f64| {
            let xexp = x.exp();
            constants[2].sub(xexp).mul(xexp)
                / constants[3].add(xexp).powi(2).mul(2_f64)
                - x.sub(constants[0]) / constants[1]
        };

        let mut a = constants[0];
        let mut b = if improvement2 > constants[3] {
            improvement2.sub(constants[3])
        } else {
            let mut x = a - constraint;
            while f(x).is_sign_negative() {
                x -= constraint;
            }
            x
        };
        let mut fa = f(a);
        let mut fb = f(b);
        while b.sub(a).abs() > accuracy {
            let c = a + a.sub(b) * fa / fb.sub(fa);
            let fc = f(c);
            if fc * fb <= 0_f64 {
                a = b;
                fa = fb;
            } else {
                fa /= 2_f64;
            }
            b = c;
            fb = fc;
        }

        self.volatility = a.div(2_f64).exp()
    }
    fn update_deviation(
        deviation: &mut f64,
        volatility: f64,
        deviation_squared: f64,
        expected_variance: f64,
        n_opponents: usize,
    ) {
        let anc = deviation_squared.add(volatility.powi(2));
        *deviation = if n_opponents > 0 {
            1_f64 / 1_f64.div(anc).add(1_f64 / expected_variance).sqrt()
        } else {
            anc.sqrt()
        };
    }

    fn update_rating(
        rating: &mut f64,
        deviation: f64,
        ops_g_factor: &[f64],
        estimated_outcome: &[f64],
        scores: &[&impl Score],
    ) {
        *rating += deviation.powi(2)
            * ops_g_factor
                .iter()
                .zip(estimated_outcome)
                .zip(scores)
                .map(|((g, e), s)| g * s.score().sub(e))
                .sum::<f64>();
    }

    /// * Adjust accuracy to be as low as needed (Good starting point would be 0.000_001).
    /// * Adjust constraint to regulate change in volatility (0.2 - 1.2 is a pretty good range)
    /// * Shift and scale adjusts the actual numbers to the desired format via the functions ´scale´ * rating + ´shift´ and ´scale´ * deviation
    pub fn evaluate_rating_period(
        &mut self,
        ops: &[&Self],
        scores: &[&impl Score],
        accuracy: f64,
        constraint: f64,
        shift: f64,
        scale: f64,
    ) {
        let mut rating = self.rating.sub(shift) / scale;
        let mut deviation = self.deviation / scale;

        let g_factor = ops
            .iter()
            .map(|&op| Self::g_factor(op.deviation / scale))
            .collect::<Vec<_>>();
        let g_factor_squared =
            g_factor.iter().map(|g| g.powi(2)).collect::<Vec<_>>();
        let e_outcomes = ops
            .iter()
            .zip(&g_factor)
            .map(|(&op, &g)| {
                Self::estimated_outcome(rating, op.rating.sub(shift) / scale, g)
            })
            .collect::<Vec<_>>();
        let v = Self::estimated_variance(&g_factor_squared, &e_outcomes);
        let e_improvement =
            Self::estimated_improvement(v, &g_factor, &e_outcomes, scores);
        let deviation2 = deviation.powi(2);
        self.update_volatility(
            deviation2,
            v,
            e_improvement,
            accuracy,
            constraint,
        );
        Self::update_deviation(
            &mut deviation,
            self.volatility,
            deviation2,
            v,
            ops.len(),
        );
        Self::update_rating(
            &mut rating,
            deviation,
            &g_factor,
            &e_outcomes,
            scores,
        );

        self.rating = rating * scale + shift;
        self.deviation = deviation * scale;
    }
}

#[cfg(test)]
mod tests {
    #![allow(unused)]
    use crate::Glicko2;

    fn init_log() {
        use colored::Colorize;
        use std::io::Write;
        pretty_env_logger::formatted_builder()
            .format(|buf, record| {
                let lvl = match record.level() {
                    log::Level::Debug => log::Level::Debug.as_str().blue(),
                    log::Level::Error => log::Level::Error.as_str().red(),
                    log::Level::Trace => {
                        log::Level::Trace.as_str().bright_blue()
                    }
                    log::Level::Info => log::Level::Info.as_str().green(),
                    log::Level::Warn => log::Level::Warn.as_str().yellow(),
                };
                writeln!(
                    buf,
                    "[{} {:5} {}/{}:{}] {}",
                    chrono::Local::now().format("%Y-%m-%dT%H:%M:%S"),
                    lvl,
                    record.module_path().unwrap_or("unknown"),
                    record.file().unwrap_or("unknown"),
                    record.line().unwrap_or(0),
                    if record.level() == log::Level::Error {
                        format!("{}", record.args()).red()
                    } else {
                        format!("{}", record.args()).into()
                    }
                )
            })
            .filter(None, log::LevelFilter::Debug)
            .init();
    }
    #[test]
    fn simple_demo() {
        // init_log();
        let mut p = Glicko2::new(1500_f64, 200_f64, 0.06);
        let ops = [
            &Glicko2::new(1400_f64, 30_f64, 0.06),
            &Glicko2::new(1550_f64, 100_f64, 0.06),
            &Glicko2::new(1700_f64, 300_f64, 0.06),
        ];
        let scores = [&1_f64, &0_f64, &0_f64];
        let constraint = 0.5;
        p.evaluate_rating_period(
            &ops, &scores, 0.000_001, constraint, 1500_f64, 173.7178,
        );
        log::info!("{:?}", p);
    }
}
