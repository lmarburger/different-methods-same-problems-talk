END {

counter = case ARGV.first
          when 'naive'   then NaiveCounter
          when 'wrapped' then WrappedCounter
          when 'locked'  then LockedCounter
          else raise 'Need an example to run: naive, wrapped, locked'
          end

simulate counter.new

}


def simulate(counter)
  loops = 1000
  loops.times.map do
    Thread.new { counter.increment }
  end.each(&:join)

  puts
  puts "Loops: #{loops}"
  puts "Count: #{counter.count}"
  puts
end


class NaiveCounter
  def initialize
    @count = 0
  end

  def count
    @count
  end

  def increment
    # Simulate `+= 1` with preemption in the critical section
    old_count = @count
    sleep 0.00001
    new_count = old_count + 1
    @count = new_count
  end
end



class WrappedCounter
  def initialize
    @counter = NaiveCounter.new
    @mutex = Mutex.new
  end

  def count
    @mutex.synchronize do
      @counter.count
    end
  end

  def increment
    @mutex.synchronize do
      @counter.increment
    end
  end
end



class LockedCounter
  def initialize
    @mutex = Mutex.new
    @count = 0
  end

  def count
    @mutex.synchronize do
      @count
    end
  end

  def increment
    @mutex.synchronize do
      old_count = @count
      sleep 0.00001
      new_count = old_count + 1
      @count = new_count
    end
  end
end
