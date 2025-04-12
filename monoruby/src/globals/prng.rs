use rand::Rng;
use rand_mt::Mt;

pub struct Prng {
    /// standard PRNG
    pub random: Mt,
    /// random seed,
    pub seed: i32,
}

impl Prng {
    pub fn new() -> Self {
        let seed = 0;
        let random = Mt::new(seed as u32);
        Self { random, seed }
    }

    pub fn init_with_seed(&mut self, seed: Option<i64>) {
        let new_seed = match seed {
            None => {
                let mut new_seed = [0u8; 4];
                if let Err(err) = getrandom::fill(&mut new_seed) {
                    panic!("from_entropy failed: {}", err);
                }
                i32::from_ne_bytes(new_seed)
            }
            Some(seed) => seed as i32,
        };
        self.seed = new_seed;
        self.random = Mt::new(new_seed as u32);
    }

    pub fn random<T>(&mut self) -> T
    where
        rand::distr::StandardUniform: rand::prelude::Distribution<T>,
    {
        self.random.random()
    }
}
