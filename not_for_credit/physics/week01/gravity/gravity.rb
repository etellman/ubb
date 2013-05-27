#!/opt/local/bin/ruby

# computes an orbit using inverse square for the gravity
# idea from Feynman Lectures 
class Orbit 
  attr_reader :x, :y, :vx, :vy, :t

  # initialize the initial values of position, velocity, and time increment
  def initialize(x, y, vx, vy, dt) 
    @x = x
    @y = y
    @dt = dt

    r = Math.sqrt(@x**2 + @y**2)
    @vx = vx + -@x/r**3 * @dt/2
    @vy = vy + -@y/r**3 * @dt/2

    @t = @dt/2
  end

  # update the positions and velocities
  def next() 
    @x += @vx * @dt
    @y += @vy * @dt

    r = Math.sqrt(@x**2 + @y**2)
    @vx = next_v(@x, @vx, r)
    @vy = next_v(@y, @vy, r)

    @t += @dt
  end

  # compute the next velocity
  def next_v(position, velocity, r)
    acceleration = -position/r**3
    return velocity + acceleration * @dt
  end
end

x0 = 0.9;
y0 = 0;

vx = 0;
vy = 0.75;

dt = 0.07;

x_error = 0.1;
y_error = 0.05;


orbit = Orbit.new(x0, y0, vx, vy, dt)
orbit.next()
 
def close(a, b, error)
  return (a-b).abs < error;
end

while (!close(orbit.x, x0, x_error) || !close(orbit.y, y0, y_error))
  orbit.next
  puts "#{orbit.x} #{orbit.y}"
end





