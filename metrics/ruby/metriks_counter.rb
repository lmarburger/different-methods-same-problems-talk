require 'atomic'

module Metriks
  # Public: Counters are one of the simplest metrics whose only operations
  # are increment and decrement.
  class Counter
    # Public: Initialize a new Counter.
    def initialize
      @count = Atomic.new(0)
    end

    # Public: Reset the counter back to 0
    #
    # Returns nothing.
    def clear
      @count.value = 0
    end

    # Public: Increment the counter.
    #
    # incr - The value to add to the counter.
    #
    # Returns nothing.
    def increment(incr = 1)
      @count.update { |v| v + incr }
    end

    # Public: Decrement the counter.
    #
    # decr - The value to subtract from the counter.
    #
    # Returns nothing.
    def decrement(decr = 1)
      @count.update { |v| v - decr }
    end

    # Public: The current count.
    #
    # Returns the count.
    def count
      @count.value
    end
  end
end



# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# From https://github.com/ruby-concurrency/atomic/blob/master/lib/atomic/fallback.rb

require 'thread'
require 'atomic/direct_update'

# Portable/generic (but not very memory or scheduling-efficient) fallback
class Atomic #:nodoc: all
  def initialize(value = nil)
    @mutex = Mutex.new
    @value = value
  end

  def get
    @mutex.synchronize { @value }
  end
  alias value get

  def set(new_value)
    @mutex.synchronize { @value = new_value }
  end
  alias value= set

  def get_and_set(new_value)
    @mutex.synchronize do
      old_value = @value
      @value = new_value
      old_value
    end
  end
  alias swap get_and_set

  def locked_compare_and_set(old_value, new_value)
    return false unless @mutex.try_lock
    begin
      return false unless @value.equal? old_value
      @value = new_value
    ensure
      @mutex.unlock
    end
    true
  end

  def compare_and_set(expected, new)
    if expected.kind_of? Numeric
      while true
        old = get

        return false unless old.kind_of? Numeric

        return false unless old == expected

        result = locked_compare_and_set(old, new)
        return result if result
      end
    else
      locked_compare_and_set(expected, new)
    end
  end
end
