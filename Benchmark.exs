defmodule Benchmark do
  # 内部合計を計算する関数（パイプ演算子を活用）
  def calc_inner_sum(u) do
    # 0からu-1までの余りの合計を計算
    base_sum = 0..(u-1) |> Enum.sum()

    # 完全なu個のグループの数と余りを計算
    quotient = div(99999, u)
    remainder = rem(99999, u)

    # 合計を計算（Elixirのパイプラインと内包表記を活用）
    total = base_sum * quotient +
            (0..remainder |> Enum.map(&rem(&1, u)) |> Enum.sum())

    total
  end

  def main([arg]) do
    u = String.to_integer(arg)
    r = :rand.uniform(10000) - 1

    # 配列の初期化と更新を最適化
    inner_sum = calc_inner_sum(u)
    a = :array.new(10000, default: inner_sum + r)

    IO.puts(:array.get(r, a))
  end
end
