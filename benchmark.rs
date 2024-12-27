use std::env;
use rand::Rng;

// 内部合計を計算する関数
fn calc_inner_sum(u: i32) -> i32 {
    // 0からu-1までの余りの合計を計算
    let base_sum: i32 = (0..u).sum();
    
    // 完全なu個のグループの数と余りを計算
    let quotient = 99999 / u;
    let remainder = 99999 % u;
    
    // 合計を計算（Rustのイテレータを活用）
    let total = base_sum * quotient +
                (0..=remainder).map(|x| x % u).sum::<i32>();
    
    total
}

fn main() {
    // コマンドライン引数の処理
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <number>", args[0]);
        std::process::exit(1);
    }

    let u: i32 = args[1].parse().expect("Invalid number");
    
    // 乱数生成
    let mut rng = rand::thread_rng();
    let r: usize = rng.gen_range(0..10000);
    
    // 内部ループの計算を1回だけ行う
    let inner_sum = calc_inner_sum(u);
    
    // ベクタの初期化を最適化（Rustのメモリ安全性を活用）
    let a: Vec<i32> = vec![inner_sum + r as i32; 10000];
    
    println!("{}", a[r]);
}
