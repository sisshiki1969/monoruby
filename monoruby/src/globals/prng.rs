use rand::Rng;
use rand_mt::Mt;

pub struct Prng {
    /// standard PRNG
    pub random: Mt,
    /// random seed (the low 32 bits actually fed to the Mersenne
    /// Twister; `Kernel#srand`'s exact Integer echo is kept separately
    /// as a Value on `Globals`).
    pub seed: i32,
}

impl Prng {
    pub fn new() -> Self {
        let seed = 0;
        let random = Mt::new(seed as u32);
        Self { random, seed }
    }

    /// Seed the PRNG. With `None`, draws a fresh nonnegative seed from OS
    /// entropy and returns it (CRuby reports the system-initialized seed
    /// as a nonnegative Integer).
    pub fn init_with_seed(&mut self, seed: Option<i64>) -> i64 {
        let new_seed = match seed {
            None => {
                let mut new_seed = [0u8; 4];
                if let Err(err) = getrandom::fill(&mut new_seed) {
                    panic!("from_entropy failed: {}", err);
                }
                (u32::from_ne_bytes(new_seed) & 0x7fff_ffff) as i32
            }
            Some(seed) => seed as i32,
        };
        self.seed = new_seed;
        self.random = Mt::new(new_seed as u32);
        new_seed as i64
    }

    pub fn random<T>(&mut self) -> T
    where
        rand::distr::StandardUniform: rand::prelude::Distribution<T>,
    {
        self.random.random()
    }
}
