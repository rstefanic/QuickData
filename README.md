# QuickData
## Create quick dummy data for SQL

Inspired by [QuickCheck](https://hackage.haskell.org/package/QuickCheck), QuickData is a way to generate dummy rows for a SQL database. Given a definition in a config file, an INSERT statement that can be used for a SQL database.

## Example
---
####Config File
```javascript 
{
  "metaData": 
  {
      "tableName": "test_table"
  ,   "rowCount" : 20
  }
  ,
  "columns": 
  [
    { 
      "columnName": "first_name"
    , "columnType": "Char"
    , "textInfo"  :
        {
          "size"     : 80
        , "textValue": "Name"
        }
    , "allowNull" : false
    }
    ,
    {
      "columnName": "last_name"
    , "columnType": "VarChar"
    , "textInfo"  : 
        {
          "size"     : 80
        , "textValue": "Name"
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

## Install
---
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