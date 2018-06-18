list = [82, 71, 12, 60, 49, 72, 36, 35, 6, 97]
puts "List: #{list}"

def list_max_loop(list)
  max = 0
  for item in list do
    if max < item
      max = item
    end
  end

  max
end

max_loop = list_max_loop(list)
puts "Loop: #{max_loop}"

def list_max_reduce(list)
  list.reduce(0) do |item, max|
    if max < item
      item
    else
      max
    end
  end
end

max_reduce = list_max_reduce(list)
puts "Reduce: #{max_reduce}"
