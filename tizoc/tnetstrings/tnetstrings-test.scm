(import (tizoc tnetstrings) (chibi test))

(test "4:true!"
      #t
      (parse-tnetstring "4:true!"))

(test "5:false!"
      #f
      (parse-tnetstring "5:false!"))

(test "5:hello,"
      "hello"
      (parse-tnetstring "5:hello,"))

(test "5:12345#"
      12345
      (parse-tnetstring "5:12345#"))

(test "0:]"
      '()
      (parse-tnetstring "0:]"))

(test "0:}"
      '()
      (parse-tnetstring "0:}"))

(test "0:,"
      ""
      (parse-tnetstring "0:,"))

(test "0:~"
      #f
      (parse-tnetstring "0:~"))

(test "25:4:list,6:value1,6:value2,]"
      '("list" "value1" "value2")
      (parse-tnetstring "25:4:list,6:value1,6:value2,]"))

(test "32:3:key,5:value,4:list,8:5:alist,]}"
      '(("list" . ("alist")) ("key" . "value"))
      (parse-tnetstring "32:3:key,5:value,4:list,8:5:alist,]}"))
