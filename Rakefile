require 'rake'

desc "link files"
task :link do
  Dir['*'].each do |file|
    next if %[Rakefile README.md].include? file

    link file
  end
end

def link(file)
  src = File.join(Dir.pwd, "#{file}")
  dest = File.join(ENV['HOME'], ".#{file}")

  if !File.exists?(dest)
    File.symlink(src, dest)
    puts "#{src} => #{dest} successfully linked"
  else
    if File.symlink?(dest)
      link = File.readlink(dest)

      if link == src
        puts "SKIPPED: #{src} => #{dest} already linked"
      else
        puts "ERROR: #{dest} is already a symlink but points to #{link}"
      end
    elsif
      puts "ERROR: #{dest} already exists but isn't a symlink"
    end
  end
end

