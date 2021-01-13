defmodule Client do
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

    def callServer(pid, nums) do
        send(pid, {qsort(nums), self()})
				listen()
    end

    def listen do
        receive do
	    		{sorted, pid} -> sorted
				end
    end
end
