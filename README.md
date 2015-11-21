# Deployment

## Install Dependencies

### MongoDB

0. Install MongoDB 3.0.x - follow [instructions](https://docs.mongodb.org/manual/tutorial/install-mongodb-on-ubuntu/)
0. Adjust configuration:

#### /etc/mongod.conf
    storage:
      dbPath: /path/to/db/
      journal:
        enabled: true
      engine: wiredTiger
    systemLog:
      destination: file
      logAppend: true
      path: /path/to/mongodb.log

#### environment (change folders as needed)
    # Paths for Scala, sbt
    export SCALA_HOME=/usr/share/scala
    export SBT_HOME=/home/ubuntu/sbt
    export PATH=$PATH:$SCALA_HOME/bin:$SBT_HOME/bin
    # Workaround for locale error in MongoDB
    export LC_ALL=C

### Scala
Install Scala 2.11.6 - follow [instructions](http://www.scala-lang.org/download/2.11.6.html)

### sbt (Scala Build Tool)
Install sbt (Scala Build Tool) 0.13.8 - download [package](https://dl.bintray.com/sbt/native-packages/sbt/0.13.8/:sbt-0.13.8.tgz), extract into folder and run `sbt` 

## Install Cuando
    git clone https://github.com/cuando-db/cuando-db.git
    cd cuando-db
    bash ./setup.sh

# Usage

### Command-line client
    cuando init  <archive> <schema> <prefs> <dims>
    cuando merge <archive> <doc>
    cuando query <archive> <query>

For example, run `examples/example.sh`

#### Arguments
##### schema
Describes schema of archive documents. See example in `examples/schema.json`.

##### prefs
Describes merge preferences. See example in `examples/prefs.json`.

##### dims
List of temporal dimensions to consider when resolving conflicts during merge. See example in `examples/dims.json`.

##### doc
Should conform to archive's schema. See example in `examples/doc.json`.

##### query
Must contain select clause, may contain project and timeslice clauses. See example in `examples/query.json`.

