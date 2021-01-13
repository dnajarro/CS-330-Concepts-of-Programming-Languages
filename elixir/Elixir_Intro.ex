defmodule Elixir_Intro do

  def fib(1) do 1 end
  def fib(2) do 1 end
  def fib(n) do
    fib(n - 1) + fib(n - 2)
  end

  def area(:rectangle, {length, height}) do length * height end
  def area(:square, side_length) do side_length * side_length end
  def area(:circle, radius) do :math.pi * radius * radius end
  def area(:triangle, {base, height}) do 0.5 * base * height end

  def sqrList(nums) do
    for n <- nums do n*n end
  end

  def calcTotals(inventory) do
    for tuple <- inventory do {elem(tuple, 0), elem(tuple, 1) * elem(tuple, 2)} end
  end

  def map(function, vals) do
    for v <- vals do function.(v) end
  end

  def qsort([]) do [] end
  def qsort(myList) do
    pivotIndex = :random.uniform(length(myList))
    smaller = for {n, counter} <- Enum.with_index(myList), n < :lists.nth(pivotIndex, myList)
      and counter != pivotIndex - 1 do n end
    larger = for {n, counter} <- Enum.with_index(myList), n >= :lists.nth(pivotIndex, myList)
      and counter != pivotIndex - 1 do n end
    qsort(smaller) ++ [:lists.nth(pivotIndex, myList)] ++ qsort(larger)
  end

	def quickSortServer() do
		receive do
			{nums, pid} ->
				send(pid, {qsort(nums), self()})
			end
			quickSortServer()
		end
end

defmodule Client do
    def callServer(pid, nums) do
        send(pid, {nums, self()})
				listen()
    end

    def listen do
        receive do
	    		{sorted, pid} -> sorted
				end
    end
end
