            20000.times do |x|
                puts x
                a=Object.new;
                a.instance_variable_set("@a", 0);
                a.instance_variable_set("@b", 1);
                a.instance_variable_set("@c", 2);
                a.instance_variable_set("@d", 3);
                a.instance_variable_set("@e", 4);
                a.instance_variable_set("@f", 5);
                a.instance_variable_set("@g", 6);
                a.instance_variable_set("@h", 7);
                a.instance_variable_set("@i", 8);
                a.instance_variable_set("@j", 9);
                a.instance_variable_set("@k", 10);
                a.instance_variable_set("@l", 11);
                a.inspect;
            end