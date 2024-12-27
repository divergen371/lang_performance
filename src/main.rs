use std::env;
use rand::Rng;

fn main() {
    let args: Vec<String> = env::args().collect();
    let u: i32 = args[1].parse().expect("Invalid number");
    
    let mut rng = rand::thread_rng();
    let r: usize = rng.gen_range(0..10000);
    
    let mut a = vec![0i32; 10000];
    
    for i in 0..10000 {
        for j in 0..100000 {
            a[i] = a[i] + (j % u);
        }
        a[i] += r as i32;
    }
    
    println!("{}", a[r]);
}
