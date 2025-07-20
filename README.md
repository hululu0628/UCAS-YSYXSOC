# ysyxSoC

Enter `make dev-init` to initialize SoC environment.

输入`make verilog`生成Verilog文件

### 文件说明
SoC中补充的内容基本都是Chisel写的，还加了一个CLINT但实际不会用到

### 存在的问题
VGA在仿真的时候表现出来的效果非常差，仿真1s能刷4帧，且仿真1s的时间很长
