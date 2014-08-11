#!/bin/bash

# This program and the accompanying materials are made available under the
# terms of the MIT license (X11 license) which accompanies this distribution.

# author: C. Bürger

if which system_profiler > /dev/null
then
	system_profiler SPHardwareDataType \
		SPMemoryDataType \
		SPParallelATADataType \
		SPSerialATADataType \
		SPParallelSCSIDataType \
		SPPCIDataType \
		SPDisplaysDataType \
		SPPowerDataType \
		SPDiagnosticsDataType > measurements/system-configuration.txt
fi
