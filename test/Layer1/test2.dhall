-- ~\~ language=Dhall filename=test/Layer1/test2.dhall
-- ~\~ begin <<lit/milkshake.md|test/Layer1/test2.dhall>>[0]
let ms = ./schema.dhall
let entry =
    { name = "entry"
    , exists =
        ''
        test $(sqlite3 test.db 'select exists(select 1 from "messages" where "id" is 1)') == 1"
        ''
    , content =
        ''
        sqlite3 test.db 'select "content" from "messages" where "id" is 1'
        ''
    } : ms.Virtual
in
    [ ms.main [ "out.txt" ]
    , { target = [ ms.Target.File "out.txt" ]
      , dependency = [ ms.Target.Generic entry ]
      , script = Some
        ''
        sqlite3 test.db 'select "content" from "messages" where "id" is 1' > out.txt
        ''
      }
    , { target = [ ms.Target.Generic entry ]
      , dependency = [] : List ms.Target
      , script = Some
        ''
        sqlite3 test.db 'create table "messages" ("id" integer primary key, "content text")'
        sqlite3 test.db 'insert into "messages" ("content") values (\\'We apologise for the inconvenience\\')'
        ''
      }
    ]
-- ~\~ end
