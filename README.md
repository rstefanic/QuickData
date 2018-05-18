# QuickData
## Create quick dummy data for SQL

Inspired by [QuickCheck](https://hackage.haskell.org/package/QuickCheck), QuickData is a way to generate dummy rows for a SQL database. Given a definition in a config file, an INSERT statement that can be used for a SQL database.

## Example

#### Config File
```javascript 
{
  "metaData": 
  {
      "tableName": "test_table",
      "rowCount" : 20
  }
  ,
  "columns": 
  [
    { 
      "columnName": "first_name"
    , "columnType": "Char"
    , "valueInfo"  :
        {
          "maxValue"  : 80
        , "textValue" : "Name"
        }
    , "allowNull" : false
    }
    ,
    {
      "columnName": "last_name"
    , "columnType": "VarChar"
    , "valueInfo"  : 
        {
          "maxValue"  : 80
        , "textValue" : "Name"
        }
    , "allowNull" : false
    }
    ,
    {
      "columnName": "age"
    , "columnType": "TinyInt"
    , "allowNull" : false
    }
  ]
}
```

#### Output
```
INSERT INTO test_table (first_name, last_name, age) VALUES (Silvie, Lanita, 218), (Roxie, Corrie, 68), (Theda, Melisse, 240), (Karole, Belva, 135), (Vallie, Margaretha, 213), (Ardine, Kipp, 152), (Fidelity, Ardyce, 203), (Jocelyn, Pamella, 63), (Christean, Rosalind, 117), (Marleah, Tobe, 67), (Daryn, Allianora, 214), (Kevina, Adela, 121), (Lucila, Perle, 69), (Daniele, Nikki, 73), (Arleta, Dulce, 161), (Shirl, Anstice, 225), (Netta, Ottilie, 88), (Alethea, Melina, 169), (Margarethe, Christel, 120), (Euphemia, Timmy, 215);
```

## How To Use

In the root folder where the project sits, there is a config.json file. Editing the config file to like the one above will output an insert statement that was specified in the config file.

The JSON file has two top level objects: the ```metadata``` (i.e. the table name and the row count) and a ```columns``` array, which defines the columns for the table. Each column must have a ```columnName```, ```columnType```, and a ```allowNull```. An optional object called ```valueInfo``` may be added to your column. If you have a ```valueInfo```, then the ```maxValue``` must be defined. The other two optional values that may be added are ```minValue``` and the ```textValue```.  

In the example above, there is an age column. If I wanted to define a minimum and maxmimum age, I could define the age column like so:

```javascript
{
  "columnName": "age"
, "columnType": "TinyInt"
, "valueInfo" :
  {
    "minValue" : 20
  , "maxValue" : 30
  }
, "allowNull" : false
}
```

This would then limit the age range from 20 to 30. If I add a minimum or maximum value that is outside of the range of that type, and error will be thrown. If no ```minValue``` is given, then the default minimum value is taken for that SQL Type. Additional, if no ```maxValue``` is given, then the maximum value is taken for that SQL Type.

In addition to the SQL number types, I can also add a minimum and maximum range for a text column. 

Adding a ```textValue``` option to a column that doesn't have text will be discarded.


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
- SqlChar
- SqlVarChar
- SqlBinary
- SqlVarBinary

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

Or you can add ```-o``` or ```--output``` followed by the file that you'd want it to be output to.
```
QuickData -o "insert.txt"
```

Using the output parameter will overwrite any file that already exists.