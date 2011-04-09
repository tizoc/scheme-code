(import (tizoc tnetstrings) (chibi test))

(test "parse 4:true!"
      #t
      (parse-tnetstring "4:true!"))

(test "parse 5:false!"
      #f
      (parse-tnetstring "5:false!"))

(test "parse 5:hello,"
      "hello"
      (parse-tnetstring "5:hello,"))

(test "parse 5:12345#"
      12345
      (parse-tnetstring "5:12345#"))

(test "parse 0:]"
      '()
      (parse-tnetstring "0:]"))

(test "parse 0:}"
      '()
      (parse-tnetstring "0:}"))

(test "parse 0:,"
      ""
      (parse-tnetstring "0:,"))

(test "parse 0:~"
      #f
      (parse-tnetstring "0:~"))

(test "parse 25:4:list,6:value1,6:value2,]"
      '("list" "value1" "value2")
      (parse-tnetstring "25:4:list,6:value1,6:value2,]"))

(test "parse 32:3:key,5:value,4:list,8:5:alist,]}"
      '(("list" . ("alist")) ("key" . "value"))
      (parse-tnetstring "32:3:key,5:value,4:list,8:5:alist,]}"))

(test "unparse #f"
      "5:false!"
      (unparse-tnetstring #f))

(test "unparse #t"
      "4:true!"
      (unparse-tnetstring #t))

(test "unparse \"string\""
      "6:string,"
      (unparse-tnetstring "string"))

(test "unparse 12345"
      "5:12345#"
      (unparse-tnetstring 12345))

(test "unparse (\"list\" \"of\" (\"values\" () 10) #f)"
      "41:4:list,2:of,17:6:values,0:]2:10#]5:false!]"
      (unparse-tnetstring '("list" "of" ("values" () 10) #f)))

(test "unparse ()"
      "0:]"
      (unparse-tnetstring '()))

(test "unparse \"\""
      "0:,"
      (unparse-tnetstring ""))
