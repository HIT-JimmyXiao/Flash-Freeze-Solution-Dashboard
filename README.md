<p align="center">
  <img src="https://img.shields.io/badge/Flash--Freeze%20Solution%20Dashboard-Shiny%20App-blue" alt="Flash-Freeze Dashboard" width="600"/>
  <br/>
  <br/>
</p>

<p align="center">
  <a href="https://github.com/HIT-JimmyXiao/Flash-Freeze-Solution-Dashboard/blob/main/LICENSE"><img alt="License" src="https://img.shields.io/badge/license-MIT-green"></a>
  <a href="https://bowenzhao917.shinyapps.io/flash-freeze-dashboard/"><img alt="Live Demo" src="https://img.shields.io/badge/demo-shinyapps.io-blue"></a>
  <a href="https://cran.r-project.org/"><img alt="R" src="https://img.shields.io/badge/R-%3E%3D4.0-276DC3"></a>
  <a href="https://python.org/"><img alt="Python" src="https://img.shields.io/badge/Python-3.8%2B-3776AB"></a>
</p>

<h4 align="center">
  <p>
    <b>简体中文</b> |
    <a href="#">English</a>
  </p>
</h4>

## 📋 项目概述

Flash-Freeze Solution Dashboard 是一个连接农户、冷冻设施与消费者的供应链可视化 Shiny 应用，展示从“农户剩余 → 冷冻批次 → 可售成品 → 订单”的全流程数据与指标。

可直接访问在线演示查看效果：

`https://bowenzhao917.shinyapps.io/flash-freeze-dashboard/`

## 📦 数据与生成

已包含：
- `flash_freeze_data.csv` — 示例数据（约 5k 行）
- `generate_data.py` — 生成脚本（可再生数据）

重新生成数据：
```bash
python generate_data.py
```

应用默认在当前目录查找 `flash_freeze_data.csv`。

## 🖥 本地运行
```r
shiny::runApp('app.R')
```

## 🚀 部署
请在私有脚本中使用你自己的 `rsconnect::setAccountInfo()`。

## 🙌 Credits
- Course: Big Data Analysis & R Language - Lab Work 3.5
- Group: 14
- Inspired by: FAO Crops & Livestock Production Dashboard (v2.3, 2025)
- Maintainer: Jimmy

