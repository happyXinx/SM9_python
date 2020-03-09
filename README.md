# SM9_python
国密SM9算法的python调用接口

国密SM9没有找到很好的Python库实现，但找到了完整的C语言实现。这时就需要用Python去调用C，而Python中的ctypes模块是Python中调用C方法比较简单的一种，ctypes模块提供了和C语言兼容的数据类型和函数来加载动态链接库（.dll文件），因此在调用时不需要对源文件做任何的修改。

参考C语言代码链接：https://github.com/Federico2014/SM9
