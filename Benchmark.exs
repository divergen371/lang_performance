defmodule Benchmark do
  def main([arg]) do
    u = String.to_integer(arg)
    r = :rand.uniform(10000) - 1
    a = :array.new(10000, default: 0)

    # 命令的なアプローチを使用
    a = Enum.reduce(0..9999, a, fn i, acc ->
      # 内部ループを直接的な計算に変更
      sum = Enum.reduce(0..99999, 0, fn j, sum ->
        sum + rem(j, u)
      end)
      :array.set(i, sum + r, acc)
    end)

    IO.puts(:array.get(r, a))
  end
end
