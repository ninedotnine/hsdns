module HSDNS.Settings (
    service_name
) where

import Network.Socket (ServiceName)

service_name :: ServiceName
service_name = "1053"
