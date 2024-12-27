use std::env;
use rand::Rng;
use rayon::prelude::*;

#[inline(always)]
fn calc_inner_sum(u: u32) -> u32 {
    // 0からu-1までの余りの合計を計算（SIMD最適化のヒント）
    #[cfg_attr(target_arch = "x86_64", target_feature(enable = "avx2"))]
    let base_sum: u32 = (0..u).sum();
    
    // 完全なu個のグループの数と余りを計算
    let quotient = 99999 / u;
    let remainder = 99999 % u;
    
    // 合計を計算（Rustのイテレータを活用）
    let total = base_sum * quotient +
                (0..=remainder).map(|x| x % u).sum::<u32>();
    
    total
}

fn main() {
    // コマンドライン引数の処理
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <number>", args[0]);
        std::process::exit(1);
    }

    let u: u32 = args[1].parse().expect("Invalid number");
    
    // 乱数生成
    let mut rng = rand::thread_rng();
    let r: usize = rng.gen_range(0..10000);
    
    // 内部ループの計算を1回だけ行う
    let inner_sum = calc_inner_sum(u);
    
    // ベクタの初期化を並列化（rayon）
    let a: Vec<u32> = (0..10000)
        .into_par_iter()
        .map(|_| inner_sum + r as u32)
        .collect();
    
    println!("{}", a[r]);
}
