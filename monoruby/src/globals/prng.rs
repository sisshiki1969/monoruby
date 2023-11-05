use rand::{Rng, SeedableRng};
use sfmt::SFMT;

pub struct Prng {
    /// standard PRNG
    pub random: SFMT,
    /// random seed,
    pub seed: <SFMT as SeedableRng>::Seed,
}

impl Prng {
    pub fn new() -> Self {
        let seed = <sfmt::SFMT as SeedableRng>::Seed::default();
        let random = SFMT::from_seed(seed);
        Self { random, seed }
    }

    pub fn init_with_seed(&mut self, seed: Option<i64>) {
        let mut new_seed = <sfmt::SFMT as SeedableRng>::Seed::default();
        match seed {
            None => {
                if let Err(err) = getrandom::getrandom(&mut new_seed) {
                    panic!("from_entropy failed: {}", err);
                }
            }
            Some(seed) => {
                for (i, byte) in (seed as i32).to_ne_bytes().iter().enumerate() {
                    new_seed[i] = *byte;
                }
            }
        };
        self.seed = new_seed;
        self.random = sfmt::SFMT::from_seed(new_seed);
    }

    pub fn gen<T>(&mut self) -> T
    where
        rand::distributions::Standard: rand::prelude::Distribution<T>,
    {
        self.random.gen()
    }
}
