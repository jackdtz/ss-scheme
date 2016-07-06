
file_code = 1

test_files = Dir["tests/s#{file_code}_*.rkt"]

test_files.each do | file |
  puts "Processing file: #{file}"
  system "racket compiler.rkt #{file}"
  puts
end