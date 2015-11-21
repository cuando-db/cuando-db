#!/usr/bin/env bash
sbt assembly
mongo localhost:27017/cuando lib/funcs.js
echo -e '#!/usr/bin/env bash\njava -jar '"$(pwd)"'/target/Cuando.jar $@\n' > cuando
chmod u+x cuando
