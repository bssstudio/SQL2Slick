SQL2Slick
================

An utility that parses SQL 'CREATE TABLE' table definitions and emits Slick table scala code.

## Usage

Compile the jar

```
sbt assembly
```

Run it (it wont work with `sbt run` because it mangles the ctrl+D)
```
java -jar target/scala-2.10/sql2slick-*.jar
```

And paste in your 'CREATE TABLE ...' table definition.

## Example

For the SQL table below

```sql
CREATE TABLE person (
    id        INT             NOT NULL,  
    first_name  VARCHAR(14)     NOT NULL,
    last_name   VARCHAR(16)     NOT NULL,
);
```

it will emit the folowing code:

```scala
/* Generated case class */
case class Person(id: Int,
  firstName: String,
  lastName: String)


/* Generated slick table class */
object PersonTable extends Table[Person]("person") {
  def id = column[Int]("id", O.NotNull)
  def firstName = column[String]("first_name", O.NotNull)
  def lastName = column[String]("last_name", O.NotNull)

  def * = id ~
    firstName ~
    lastName <> ( table => Person(table._1, 
        table._2, 
        table._3),
  
      (obj: Person) => Some((obj.id, 
        obj.firstName, 
        obj.lastName))
    )
}

```

## Compatibility

SQL2Slick is currently tested with MySQL (MariaDB) table definitions and currently supports the following types:

  - INTEGER, TINYINT, MEDIUMINT, BIGINT
  - VARCHAR, TEXT
  - DOUBLE
  - DECIMAL
  - BLOB 
  - BINARY, VARBINARY
  - DATETIME, DATE, TIME, TIMESTAMP (depends on JodaTime)
  - variations of types mentioned above
