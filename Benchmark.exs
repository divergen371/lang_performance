defmodule Benchmark do
  # 内部ループの計算を行う関数
  defp calc_inner_sum(u) do
    # 0からu-1までの余りの合計を計算
    base_sum = Enum.sum(0..(u-1))

    # 完全なu個のグループの数と余りを計算
    quotient = div(99999, u)
    remainder = rem(99999, u)

    # 合計を計算
    remainder_sum = 0..remainder
    |> Enum.map(&rem(&1, u))
    |> Enum.sum()

    base_sum * quotient + remainder_sum
  end

  # 配列の一部を計算する関数
  defp calculate_chunk(start, size, value) do
    List.duplicate(value, size)
  end

  def main([arg]) do
    try do
      u = String.to_integer(arg)

      # 乱数生成
      r = :rand.uniform(10000) - 1

      # 内部ループの計算を1回だけ行う
      inner_sum = calc_inner_sum(u)
      value = inner_sum + r

      # 並列処理を使用して配列を初期化
      chunk_size = 1000
      result = 0..9
      |> Enum.map(fn i ->
        Task.async(fn ->
          calculate_chunk(i * chunk_size, chunk_size, value)
        end)
      end)
      |> Task.await_many(:infinity)
      |> List.flatten()
      |> :array.from_list()

      # 結果を出力
      IO.puts(:array.get(r, result))
      System.halt(0)
    rescue
      _ ->
        IO.puts(:stderr, "Usage: elixir Benchmark.exs <number>")
        System.halt(1)
    end
  end

  def main(_) do
    IO.puts(:stderr, "Usage: elixir Benchmark.exs <number>")
    System.halt(1)
  end
end

# スクリプトとして実行された場合のエントリーポイント
System.argv() |> Benchmark.main()
