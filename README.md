# hululu's ysyxSoC

Enter `make dev-init` to initialize SoC environment.

### 接入ysyxSoC
将CPU.scala中的BlackBox模块设置为ysyxCPU，将Top.scala中的顶层模块设置为Top（Elaborate当然也要改动），将Makefile的V_FILE_GEN值设 为Top
