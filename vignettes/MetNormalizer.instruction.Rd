---
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<img src = "http://ow5z7hup5.bkt.clouddn.com/new%20new%20logo2.png" class = "img-topic" width = "800" align="middle" />

<h1>**Instruction of MetDNA (version 0.99.12)**</h1>
<h3>**Xiaotao Shen, Zhengjiang Zhu**</h3>
<h3>`r format(Sys.Date())`</h3>

***

### **<font color=red >问题报告</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> <font color=red >如果有任何问题出现，请点击[此处](http://shenxt.me/2017/09/09/2017-09-09-metdna-blog/)给我留言。留言要包括下面三个部分：</font> </p> </h4>

* #### <font color=red >处理文件地址；</font>

* #### <font color=red >运行的代码截图；</font>

* #### <font color=red >出现问题的报错信息截图。</font>


### **<font color=red >常见错误总结</font>**

* <h4> <p style="line-height: 1.5em;" align = "justify">sample.info的两列列名必须是sample.name和group。</p> </h4>
* <h4> <p style="line-height: 1.5em;" align = "justify">如果有正负离子模式数据，必须保证正负离子模式的样品名完全相同。</p> </h4>

### **<font color=red >版本日志</font>**

### **<font color=royalblue >Version0.99.12</font>**

* <h4> <p style="line-height: 1.5em;" align = "justify">可以正负离子模式同时处理。</p> </h4>
* <h4> <p style="line-height: 1.5em;" align = "justify">输出运行日志run.log。</p> </h4>
* <h4> <p style="line-height: 1.5em;" align = "justify">换了logo。</p> </h4>



***

### **<font color=lightseagreen >MetDNA处理步骤</font>**

#### MetDNA一共处理过程一共分为了以下几个步骤：

* #### 数据检查；

* #### 二级谱图匹配：使用标准二级谱图数据库进行鉴定；

* #### 基于代谢反应网络的代谢物鉴定；

* #### 紊乱网络分析；

* #### 生成分析报告。

*** 

### **<font color=lightseagreen >Ⅰ数据准备</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> MetDNA需要准备的数据包括一级数据peak table(csv格式)，二级数据(mgf格式或者msp)和样品信息sample.info(csv格式)。点击下载[正离子demo数据](http://oxr5qv74w.bkt.clouddn.com/MetDNA.demo.data.POS.rar)和[负离子demo数据](http://oxr5qv74w.bkt.clouddn.com/MetDNA.demo.data.NEG.rar)。</p> </h4>


#### <font color=red >注意：</font>

<h4> <p style="line-height: 1.5em;" align = "justify"> 如果正负离子模式数据都有，最后想使用正负离子模式的鉴定结果进行pathway分析，那么需要保证正负离子模式的样品名称保持一致，也就是同一个样品在正负离子模式数据中一样 </p> </h4>



 **Table 1: demo数据信息**

组别            | 个数          |   含义
--------------- | ------------- | ------------
QC              |   8           | QC
W03             |   10          | 野生型3天
W30             |   10          | 野生型30天
E03             |   10          | 突变型E3天
E30             |   10          | 突变型E30天
P03             |   10          | 突变型P3天
P30             |   10          | 突变型P30天

                                                                                                             
### **<font color=lightseagreen >1. 一级数据(data.csv)</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> 一级数据可以是使用XCMS，MZmine，MS-DIAL或者其他软件处理之后的数据。第一列必须为代谢物峰的名字，"name"，第二列为"mz"，第三列为保留时间(RT)，且单位必须为秒，其他为样品的峰强度，除此之外不需要其他任何信息，**且前三列命名必须为"name"，"mz"， "rt"**。 </p> </h4>


![**Figure 1: 一级数据peak table格式示例**](http://oy0flpoi6.bkt.clouddn.com/data.example.png)


### **<font color=lightseagreen >2. 二级数据</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> 二级质谱原始数据可以是使用样品采集的DDA，DIA或者targeted MS/MS数据。对于DDA数据来说，也可以是分段采集的二级数据。对于DDA和targeted MS/MS数据来说，需要将质谱原始二级数据使用ProteoWizard软件转为mgf格式，转换时参数设置参考下图。对于DIA数据来说，可以使用MS-DIAL处理之后，将其输出的msp格式文件直接拿来使用。 </p> </h4>


![**Figure 2: ProteoWizard参数设置**](http://oy0flpoi6.bkt.clouddn.com/ProteoWizard.png)

### **<font color=lightseagreen >3. 样品信息(sample.info)</font>**


<h4> <p style="line-height: 1.5em;" align = "justify"> 样品信息是样品的分组信息。第一列是样品名，命名为，"sample.name"，第二列是样品的分组信息，命名为，"group"。样品信息为csv文件，命名为sample.info.csv。 </p> </h4>

![**Figure 3：样品信息示例**](http://oy0flpoi6.bkt.clouddn.com/sample.info.png)

***

### **<font color=lightseagreen >Ⅱ 数据整理</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> 如果是正离子数据，请建立一个新的文件夹，可以随意命名，如"POS"，如果是负离子数据，请建立一个新的文件夹，可以随意命名，如"NEG"，然后将一级数据**(可以随意命名，如data.csv)**，二级数据(mgf或者msp格式)和样品信息**(可以随意命名，如sample.info.csv)**放置于正离子或者负离子文件夹下。<font color=red>并将正离子数据和负离子数据的上一级路径，设置为工作路径</font>。现在*MetDNA*部署在小服务器上，因此需要将数据放在小服务器中**(注意是labdata文件夹)**。 </p> </h4>


![**Figure 4: 将所需数据放置在文件夹中**](http://oy0flpoi6.bkt.clouddn.com/%E6%95%B0%E6%8D%AE%E6%95%B4%E7%90%86.png)

***

### **<font color=lightseagreen >Ⅲ 数据处理</font>**


<h4> <p style="line-height: 1.5em;" align = "justify"> 所有的步骤可以使用一个函数*MetDNA*全部完成。</p> </h4>


#### 设置工作路径并加载*MetDNA*。

```{r, eval=FALSE}
#设置工作路径并加载MetDNA
setwd("/mnt/data/samba/labdata/workreport/Shen Xiaotao/demo/fly")
library(MetDNA)
```

#### 运行*MetDNA*处理数据。

```{r, eval=FALSE}
MetDNA(ms1.data.pos = "data.csv",
       ms1.data.neg = "data.csv",
       sample.info.pos = "sample.info.csv",
       sample.info.neg = "sample.info.csv",
       pos.path = "/mnt/data/samba/labdata/workreport/Shen Xiaotao/demo/fly/POS",
       neg.path = "/mnt/data/samba/labdata/workreport/Shen Xiaotao/demo/fly/NEG",
       polarity = "positive",
       column = "hilic",
       ce = "30",
       use.default.md = TRUE,
       group = c("W03", "W30"),
       uni.test = "t",
       correct = TRUE,
       p.cutoff = 0.01,
       species = "dme",
       dn.analysis = FALSE,
       pathway.enrichment = TRUE)
```

> **参数含义如下：**

* <h4> <p style="line-height: 1.5em;" align = "justify"> ms1.data.pos：正离子数据MS1 data的名字，默认为"data.csv"，如果只有负离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> ms1.data.neg：负离子数据MS1 data的名字，默认为"data.csv"，如果只有正离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> sample.info.pos：正离子数据样品信息的名字，默认为"sample.info.csv"，如果只有负离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> sample.info.neg：负离子数据样品信息的名字，默认为"sample.info.csv"，如果只有正离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> pos.path：正离子数据的放置路径，注意，一定要是绝对路径。如果只有负离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> pos.path：负离子数据的放置路径，注意，一定要是绝对路径。如果只有正离子数据，则该参数不用关注。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> polarity：数据采集极性，"positive"，"negative"或者"both"。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> column：使用的柱子类型，"hilic"或者"rp"。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> ce：二级采集的碰撞能量，支持"10"，"15"，"20"，"25"，"30"，"35"，"35,15" (35±15)，"40"， "45"，"50"，"55"，"60"，"65"，"70"。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> use.default.md：进行保留时间预测模型建立时，是否使用默认的分子描述符，如果设置为FALSE，则会根据你的数据自动选择分子描述符。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> group：要对哪些分组的样品进行分析，注意，计算fold change时，使用后面的样品除以前面的样品。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> uni.test：单变量分析的方法，"t"，Student t test；"wilcox"，Wilcox test。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> correct：是否需要对p值进行FDR校正。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> p.cutoff：选择dysregulated peak时的p值cutoff。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> species：所研究样品的物种来源，"dme"，果蝇；"hsa"，人类；"mmu"，小鼠；"rat"，大鼠，"bta"，牛；"gga"，Gallus gallus (鸡)；"dre"，Danio rerio (斑马鱼)；"cel"，Caenorharomyces elegans (线虫)；"sce"，Saccharomyces cerevisaiae (酵母)； "ath"，Arabidopsis thaliana (拟南芥)；"smm"，Schistosoma mansoni；"pfa"，Plasmodum falciparum 3D7；"tbr"，Trypanosoma brucei；"eco"， Escherichia coli K-12 MG1655(大肠杆菌)；"ppu"，Pseudomonas putida KT2440；"syf"，Synechococcus elongatus。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> dn.analysis：是否进行紊乱网络分析，<font color=red >如果正负离子模式数据都有，那么需要将其设置为FALSE，如果只有一个模式的数据，那么设置为TRUE。</font></p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> pathway.enrichment：是否使用差异代谢物对应的代谢物直接进行pathway enrichment analysis，现在默认的是如果polarity为"positive"或者"negative"，则不进行分析，如果polarity为"both"，则进行分析。</p> </h4>

#### **<font color=red >注意：</font>**

* <h4> <p style="line-height: 1.5em;" align = "justify"> 因为现在只能对两组数据进行比较，因此group参数只能写两个，如果有三组数据，如A，B和C组，那么需要分来两次处理，第一次先比较A和B，即将group设置为c("A", "B")，然后运行*MetDNA*，然后将已经得到的结果中的"Dysregulated_network_analysis_result"，"Pathway_enrichment_analysis_result"和"Analysis_report"重新命名，否则下次运行这两个文件夹中的内容会被覆盖掉，然后将group设置为c("A", "C")，然后再次运行*MetDNA*。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> 进行pathway enrichment分析时，可以自定义用于pathway分析的差异代谢物峰及其注释结果。在MetDNA中，默认是按照p值进行筛选差异代谢物峰，并使用差异代谢物峰的所有注释结果进行后续的通路分析，如果想自己按照其他方式筛选差异代谢物峰及其对应的注释，那么需要将自己筛选到的代谢物峰及其注释命名为"marker.csv"，然后将其放置在"POS"或者"NEG"文件夹中，重新运行MetDNA函数即可。</p> </h4>



***

### **<font color=lightseagreen >Ⅳ 运行结果</font>**

<h4> <p style="line-height: 1.5em;" align = "justify"> *MetDNA*函数运行结束之后，所有的运行结果都存放在设置的路径中，包含二级谱图匹配鉴定结果，MRN注释结果，dysregulated network分析结果，pathway enrichment结果以及分析报告。如图5所示。[点击此处观察分析结果结构图](https://www.processon.com/view/link/59e71c36e4b040dc8507df21)。 </p> </h4>

![**Figure 5: 运行结果**](http://oy0flpoi6.bkt.clouddn.com/analysis.result.png)

#### **1 MetDNA.parameters.csv**

#### 记录此次运行所使用的参数。

#### **2 MS2_match_result (二级谱图匹配结果)**

* <h4> <p style="line-height: 1.5em;" align = "justify"> **ms2.match.annotation.result.csv**：二级谱图匹配之后的结果，与**MetAnalyzer**处理之后的结果相同；</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> **MS2_match_spectra**：包含了所有二级谱图匹配结果图。 </p> </h4>


#### **3 MRN_annotation_result (基于metabolic reacion network注释结果)**

* <h4> <p style="line-height: 1.5em;" align = "justify">**MRN.annotation.result.csv：**基于MRN的每一个peak的注释结果(Figure 6)。</p> </h4>


* <h4> <p style="line-height: 1.5em;" align = "justify">**Seed_Neighbor_MS2_match_spectra：**包含的是每个Seed和他neighbor之间的二级谱图匹配结果。</p> </h4>

![**Figure 6: MRN annotation result**](http://oy0flpoi6.bkt.clouddn.com/mrn.annotation.result1.png)

####  <font color = "red" >**MRN.annotation.result.csv**其中的一些列的含义：</font>

* <h4> <p style="line-height: 1.5em;" align = "justify">**Annotation.type：**该peak的该注释的类型，其中seed代表是从二级谱图鉴定得到的，isotopeAnnotation是指是同位素峰注释得到的，adductAnnotation是指加合物峰注释得到的，metAnnotation是指邻近代谢物注释得到的；</p> </h4>

*  #### **annotation.from.ID：**该peak的该注释来自于哪个metabolite(ID);
*  #### **annotation.from.peak：**该peak的该注释来自于哪个peak;
*  #### **ID：**注释代谢物结果的KEGG ID;
*  #### **compound.name：**注释结果的名字；
*  #### **isotope：**同位素信息；
*  #### **adduct：**加合物信息；
*  #### **Formula：**化学结构式；
*  #### **score：**注释打分；
*  #### **peak.group：**peak group；
*  #### **confidence：**对注释的peak group打分。


#### **4 Dysregulated_network_analysis_result**

<h4> <p style="line-height: 1.5em;" align = "justify"> 1) volcano.plot是选取差异代谢物峰的火山图。每个代谢物峰的p值和fold change值可以从DNA.module.annotation.result.pos.csv中得到，或者在intermediate_data文件夹中有两个R文件，分别是p.value和fold.change。</p> </h4>


<h4> <p style="line-height: 1.5em;" align = "justify"> 2) DNA.module.annotation.result.pos.csv是通过dysregulated modules对注释结果进行筛选以及KEGG database注释之后的注释结果。其中每列的含义可以参考**MRN.annotation.result.csv**的说明。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 3) DNA_module_information：module的一些结果，该文件夹结果暂时用不到，可以不看。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 4) DNA_function_annotation：紊乱网络的定性分析和定量分析结果。具体内容如下：</p> </h4>

![**Figure 7: MRN annotation result**](http://oy0flpoi6.bkt.clouddn.com/DNA_functional_annotation.png)

* <h4> <p style="line-height: 1.5em;" align = "justify">Cytoscape_data：dysregulated network用于cytoscape作图的文件。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> Dysregulated_network_boxplot：每个pathway的定量信息box plot。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> Dysregulated_network_heatmap：每个pathway的定量信息heatmap。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> Dysregulated_network_heatmap：每个pathway的定量信息heatmap。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> Quantitative_information：pathway的定量信息，以及每个代谢物对应的peak的信息。</p> </h4>

* <h4> <p style="line-height: 1.5em;" align = "justify"> DNA.pathway.enrichmetn.result.csv：dysregulated network通路富集结果。</p> </h4>

#### **5 Pathway_enrichment_analysis_result**

<h4> <p style="line-height: 1.5em;" align = "justify"> 1) volcano.plot是选取差异代谢物峰的火山图。</p> </h4>


<h4> <p style="line-height: 1.5em;" align = "justify"> 2) Pathway.enrichment.analysis.csv是使用差异代谢物峰对应的注释直接进行通路富集分析得到的富集结果。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 3) Pathway.enrichment.MSEA.pdf是pathway富集分析的结果。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 4) Pathway.enrichment.overview.pdf是pathway富集分析的结果的展示。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 4) Pathway.heatmap.pdf是pathway定量结果的热图展示。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 5) Boxplot文件夹：是每个通路的定量box plot展示。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 6) Heatmap文件夹：是每个通路的定量heat map展示。</p> </h4>

<h4> <p style="line-height: 1.5em;" align = "justify"> 7) Quantitative_information：pathway的定量信息，以及每个代谢物对应的peak的信息。</p> </h4>


### **<font color=lightseagreen >6 Analysis_report</font>**

#### 对数据处理分析结果的总结。输出的结果存放在Analysis_report文件夹内。包括一份html格式的[分析报告](http://rpubs.com/Jasper/metdna_analysis_report)。


