#!/usr/bin/env ruby

c = 0;
loop do
	puts c
	if (c % 3) == 0
		s = `curl http://localhost:8080/clamorous/subscribe/wait/0`
	else
		s = `curl http://localhost:8080/clamorous/subscribe/wait/new`
	end
	puts s
	puts s.lines.count
	sleep 1
	c += 1
end

