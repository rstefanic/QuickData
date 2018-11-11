# QuickData
## Create quick dummy data for SQL

Inspired by [QuickCheck](https://hackage.haskell.org/package/QuickCheck),
QuickData is a way to generate dummy rows for a SQL database. Given a definition
in a config file, an INSERT statement that can be used for a SQL database will
be generated.

## Example

#### Config File
```yaml
for test_table with 20 records

Column:
        Name: first_name
        Nullable: false
        Type: Text
                Max: 80
                TextValue: name

Column:
        Name: last_name
        Nullable: false
        Type: Text
                Max: 80
                TextValue: name

Column:
        Name: Age
        Nullable: true
        Type: TinyInt
                Min: 20
                Max: 80
```

#### Output
```
INSERT INTO test_table (first_name, last_name, age) VALUES (Shayne, Cristina, 70), (Orly, Jeni, 42), (Tera, Cybil, NULL), (Leslie, Frederica, 64), (Wren, Jacinthe, NULL), (Melisa, Nissie, 71), (Reeta, Rosita, NULL), (Nelia, Keely, NULL), (Vally, Vevay, 78), (Arda, Rivalee, NULL), (Almire, Michaelina, 57), (Ophelie, Britni, 52), (Kathe, Moyna, NULL), (Brandie, Liuka, NULL), (Dorothea, Ailyn, NULL), (Gisele, Bianka, NULL), (Shirl, Thekla, 30), (Martha, Arlie, NULL), (Elonore, Genny, 23), (Theresita, Johnna, NULL);
```

## How To Use

In the root folder where the project sits, there is a config.qd file (You can
also specifiy which .qd you'd like read by passing the ```-i``` argument. More
details below). 
Editing the config file like the one above will output an insert statement that was specified in the config file.

The file is a .qd file where the first line is the ```metadata``` (i.e. the
table name and the row count). The rest is of the file are columns marked by a
```Column``` which define the columns for the table. For every field within a
given column, it must be indented 1 line with a tab. Each ```Column``` must include a
```Name```, a true or false saying whether or not the column is ```Nullable```
and a SQL Type marked by ```Type```. Each column must be defined in that order.

For each SQL Type, depending on the type, you can add an optional information to
go along with the type that type to describe how the data for that column should
be defined by adding an additional tab under the ```Type```.

The language markup language for .qd is now case sensitive.

In the example above, there is an age column. If I wanted to define a minimum
and maxmimum age, then I could define the age column like this:

```yaml
Column:
        Name: kids_age
        Nullable: false
        Type: TinyInt
                Min: 0
                Max: 18
```

This would then limit the age range from 0 to 18. If I add a minimum or maximum
value that is outside of the range of that type, an error will be thrown. If no ```min``` is given, then the default minimum value is taken for that SQL Type. Additional, if no ```max``` is given, then the maximum value is taken for that SQL Type.

In addition to the SQL number types, I can also add a minimum and maximum range for a text column. 

Adding a ```TextValue``` option to a column that doesn't have text will be discarded. If the SQL type supports unicode, then the randomized data will Greek.

Currently, QuickData recognizes the following SQL Types:

- BigInt
- Int
- SmallInt
- TinyInt
- Bit
- Float
- Date
- DateTime
- Text
- NText
- Char
- NChar
- VarChar
- NVarChar
- Binary
- VarBinary

## Install

**Clone and install from stack**
```
git clone https://github.com/rstefanic/QuickData
cd QuickData
stack update
stack install
```

**Clone and install from cabal**
```
git clone https://github.com/rstefanic/QuickData
cd QuickData
cabal update
cabal install
```
## Running The Program

You can either run it and have the INSERT statement printed to the console.
```
QuickData
```

You can add ```-o``` or ```--output``` followed by the name of the file that
you'd want your output written to.
```
QuickData -o output.sql
```

Or you can add ```-i``` or ```--input``` followed by the name of the file you'd
like the INSERT statement to be written to.
```
QuickData -i input.qd
```

Using the output parameter will overwrite any file that already exists.
