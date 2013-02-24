module Pianola.Model.Swing.Driver (
    simpleSwingDriver,
    module Pianola.Util,
    module Pianola.Pianola,
    module Pianola.Protocol,
    module Pianola.Protocol.IO,
    module Pianola.Pianola.Driver,
    module Pianola.Model.Swing
) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
import Data.Stream.Infinite
import Control.Error
import Pianola.Util
import Pianola.Protocol
import Pianola.Protocol.IO
import Pianola.Pianola
import Pianola.Pianola.Driver
import Pianola.Model.Swing
import Pianola.Model.Swing.Protocol (snapshot)

simpleSwingDriver :: Endpoint -> Pianola Protocol LogEntry (GUI Protocol) a -> Stream FilePath -> EitherT DriverError IO a
simpleSwingDriver = simpleDriver snapshot
