#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Flash-Freeze Solution Dashboard - 数据生成器
生成完整的模拟数据CSV文件，供Shiny应用读取
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import random

# 设置随机种子以便复现
np.random.seed(42)
random.seed(42)

print("=" * 60)
print("  Flash-Freeze Solution Dashboard - 数据生成")
print("=" * 60)

# ============================================================
# 1. 基础数据：区域
# ============================================================
regions_data = [
    ['R-North', 'North Region', 25, 35, 6],
    ['R-South', 'South Region', 18, 42, 8],
    ['R-East', 'East Region', 30, 28, 5],
    ['R-West', 'West Region', 22, 30, 4],
    ['R-Central', 'Central Region', 12, 15, 2]
]

regions_df = pd.DataFrame(regions_data, columns=[
    'region_id', 'region_name', 'avg_distance_km', 'farmers_count', 'facilities_count'
])

print("\n✓ 区域数据生成完成")

# ============================================================
# 2. 农户数据
# ============================================================
farmer_names = [
    'Zhang Wei', 'Li Ming', 'Wang Fang', 'Chen Jing', 'Liu Yang',
    'Zhao Xin', 'Sun Lei', 'Zhou Min', 'Wu Qiang', 'Xu Hua',
    'Ma Lin', 'Zhu Hong', 'Huang Tao', 'Guo Yan', 'Tang Bo',
    'He Xue', 'Song Kai', 'Cui Mei', 'Jiang Feng', 'Deng Li',
    'Fan Wei', 'Xie Jun', 'Pan Yue', 'Luo Ning', 'Qian Rui',
    'Shi Gang', 'Ren Jie', 'Zheng Lan', 'Kong Yu', 'Cao Xia'
]

farmers_list = []
for i, name in enumerate(farmer_names, 1):
    region = random.choice(regions_df['region_id'].tolist())
    farmers_list.append([
        f'F{i:03d}',
        f'Farmer {name}',
        region,
        f'138{random.randint(10000000, 99999999)}'
    ])

farmers_df = pd.DataFrame(farmers_list, columns=[
    'farmer_id', 'farmer_name', 'region_id', 'contact'
])

print(f"✓ 农户数据生成完成: {len(farmers_df)} 位农户")

# ============================================================
# 3. 冷冻设施数据
# ============================================================
facilities_list = [
    ['C001', 'FrostHub North A', 'R-North', 500],
    ['C002', 'FrostHub North B', 'R-North', 450],
    ['C003', 'IceBox South Main', 'R-South', 600],
    ['C004', 'IceBox South Branch', 'R-South', 400],
    ['C005', 'ColdChain East', 'R-East', 480],
    ['C006', 'QuickFreeze West A', 'R-West', 420],
    ['C007', 'QuickFreeze West B', 'R-West', 380],
    ['C008', 'Central FreezeHub', 'R-Central', 550],
    ['C009', 'Eastern Frost Facility', 'R-East', 500],
    ['C010', 'Southern Ice Plant', 'R-South', 520]
]

facilities_df = pd.DataFrame(facilities_list, columns=[
    'facility_id', 'facility_name', 'region_id', 'capacity_per_day'
])

print(f"✓ 冷冻设施数据生成完成: {len(facilities_df)} 个设施")

# ============================================================
# 4. 水果库存数据 (农户surplus)
# ============================================================
fruit_types = ['Apple', 'Strawberry', 'Blueberry', 'Orange', 'Mango', 'Grape', 'Cherry']
base_date = datetime(2025, 10, 15)

inventory_list = []
inv_id = 1

for day in range(30):  # 30天的数据（增加时间跨度）
    current_date = base_date + timedelta(days=day)
    
    # 每天随机选择一些农户有surplus（增加活跃农户数）
    active_farmers = random.sample(farmers_df['farmer_id'].tolist(), 
                                   random.randint(20, 28))
    
    for farmer_id in active_farmers:
        # 每个农户可能有1-3种水果
        num_fruits = random.randint(1, 3)
        selected_fruits = random.sample(fruit_types, num_fruits)
        
        for fruit in selected_fruits:
            surplus_kg = random.randint(50, 300)
            
            # 不同水果的基础价格
            base_prices = {
                'Apple': 4.5, 'Strawberry': 10.0, 'Blueberry': 20.0,
                'Orange': 5.0, 'Mango': 15.0, 'Grape': 12.0, 'Cherry': 18.0
            }
            price = base_prices[fruit] + random.uniform(-1, 1)
            
            inventory_list.append([
                f'I{inv_id:04d}',
                farmer_id,
                fruit,
                surplus_kg,
                round(price, 2),
                current_date.strftime('%Y-%m-%d')
            ])
            inv_id += 1

inventory_df = pd.DataFrame(inventory_list, columns=[
    'inventory_id', 'farmer_id', 'fruit_type', 'surplus_kg', 'price_per_kg', 'created_at'
])

print(f"✓ 农户库存数据生成完成: {len(inventory_df)} 条记录")

# ============================================================
# 5. 冷冻批次数据 (pipeline)
# ============================================================
statuses = [
    'Awaiting Pickup',
    'In Transit',
    'At Freezing Facility',
    'Freezing in Progress',
    'Quality Check',
    'Ready for Sale'
]

batches_list = []
batch_id = 1001

# 从库存中随机选择更多批次（提高转化率到60%）
num_batches = min(int(len(inventory_df) * 0.6), len(inventory_df))
selected_inventories = random.sample(inventory_df['inventory_id'].tolist(), 
                                     num_batches)

for inv_id in selected_inventories:
    inv_row = inventory_df[inventory_df['inventory_id'] == inv_id].iloc[0]
    
    # 匹配同区域的设施
    farmer_region = farmers_df[farmers_df['farmer_id'] == inv_row['farmer_id']]['region_id'].iloc[0]
    region_facilities = facilities_df[facilities_df['region_id'] == farmer_region]['facility_id'].tolist()
    
    if not region_facilities:
        region_facilities = facilities_df['facility_id'].tolist()
    
    facility = random.choice(region_facilities)
    
    # 批次数量略小于库存
    qty = min(inv_row['surplus_kg'], random.randint(50, 250))
    
    # 随机状态（提高"Ready for Sale"的比例到40%）
    status_weights = [0.1, 0.1, 0.15, 0.15, 0.1, 0.4]  # 最后40%是Ready for Sale
    status = random.choices(statuses, weights=status_weights, k=1)[0]
    
    # 时间戳
    start_time = datetime.strptime(inv_row['created_at'], '%Y-%m-%d') + timedelta(hours=random.randint(2, 24))
    update_time = start_time + timedelta(hours=random.randint(1, 48))
    
    batches_list.append([
        f'B{batch_id:04d}',
        inv_id,
        facility,
        status,
        qty,
        start_time.strftime('%Y-%m-%d %H:%M'),
        update_time.strftime('%Y-%m-%d %H:%M')
    ])
    batch_id += 1

batches_df = pd.DataFrame(batches_list, columns=[
    'batch_id', 'source_inventory_id', 'facility_id', 'status', 
    'qty_kg', 'start_at', 'last_update_at'
])

print(f"✓ 冷冻批次数据生成完成: {len(batches_df)} 个批次")

# ============================================================
# 6. 冷冻成品数据 (已完成的批次 -> 袋装产品)
# ============================================================
products_list = []
prod_id = 9001

# 只有状态为 "Ready for Sale" 的批次才有成品
ready_batches = batches_df[batches_df['status'] == 'Ready for Sale']

for _, batch in ready_batches.iterrows():
    # 找到对应的水果类型
    inv_row = inventory_df[inventory_df['inventory_id'] == batch['source_inventory_id']].iloc[0]
    fruit = inv_row['fruit_type']
    
    # 每个批次生成1-3个不同规格的产品（增加成品密度）
    bag_sizes = [0.5, 1.0, 1.5]
    num_products = random.randint(1, 3)
    selected_sizes = random.sample(bag_sizes, num_products)
    
    for bag_size in selected_sizes:
        # 计算袋数
        bags = int(batch['qty_kg'] / bag_size / num_products)  # 分配到各规格
        
        if bags < 10:  # 至少10袋
            bags = random.randint(10, 50)
        
        # 成品价格（基于原料价加成）
        base_price = inv_row['price_per_kg'] * bag_size * 1.3  # 30%加成
        price_per_bag = round(base_price + random.uniform(-0.5, 1.0), 2)
        
        # 发布时间
        released = datetime.strptime(batch['last_update_at'], '%Y-%m-%d %H:%M') + timedelta(hours=2)
        
        products_list.append([
            f'P{prod_id:04d}',
            batch['batch_id'],
            fruit,
            bag_size,
            bags,
            price_per_bag,
            released.strftime('%Y-%m-%d %H:%M')
        ])
        prod_id += 1

products_df = pd.DataFrame(products_list, columns=[
    'product_id', 'batch_id', 'fruit_type', 'bag_size_kg', 
    'bags_available', 'price_per_bag', 'released_at'
])

print(f"✓ 冷冻成品数据生成完成: {len(products_df)} 个产品")

# ============================================================
# 7. 订单数据
# ============================================================
order_statuses = ['Pending', 'Paid', 'Shipped', 'Completed', 'Cancelled']
buyer_types = ['Retailer', 'Consumer', 'Restaurant', 'Wholesaler']

orders_list = []
order_id = 7001

# 从产品中随机生成订单（增加订单密度）
if len(products_df) > 0:
    # 每个产品生成2-5个订单
    for _, product in products_df.iterrows():
        num_orders = random.randint(2, 5)
        for _ in range(num_orders):
            # 订单数量
            bags_ordered = random.randint(5, min(50, product['bags_available']))
            
            # 订单状态（更真实的分布）
            status_probs = [0.1, 0.25, 0.2, 0.4, 0.05]  # Pending, Paid, Shipped, Completed, Cancelled
            order_status = random.choices(order_statuses, weights=status_probs, k=1)[0]
            
            # 下单时间
            order_time = datetime.strptime(product['released_at'], '%Y-%m-%d %H:%M') + \
                         timedelta(hours=random.randint(1, 72))
            
            buyer = random.choice(buyer_types)
            
            orders_list.append([
                f'O{order_id:04d}',
                product['product_id'],
                bags_ordered,
                order_status,
                order_time.strftime('%Y-%m-%d %H:%M'),
                buyer
            ])
            order_id += 1

orders_df = pd.DataFrame(orders_list, columns=[
    'order_id', 'product_id', 'bags_ordered', 'order_status', 
    'order_at', 'buyer_type'
])

print(f"✓ 订单数据生成完成: {len(orders_df)} 条订单")

# ============================================================
# 8. 合并所有数据到单一CSV
# ============================================================

# 添加表标识列
regions_df['table_type'] = 'region'
farmers_df['table_type'] = 'farmer'
facilities_df['table_type'] = 'facility'
inventory_df['table_type'] = 'inventory'
batches_df['table_type'] = 'batch'
products_df['table_type'] = 'product'
orders_df['table_type'] = 'order'

# 为了方便合并，先把每个表转成统一格式（添加缺失列为NA）
all_columns = set()
for df in [regions_df, farmers_df, facilities_df, inventory_df, 
           batches_df, products_df, orders_df]:
    all_columns.update(df.columns)

all_columns = sorted(list(all_columns))

def pad_dataframe(df, columns):
    for col in columns:
        if col not in df.columns:
            df[col] = pd.NA
    return df[columns]

# 统一列
regions_padded = pad_dataframe(regions_df.copy(), all_columns)
farmers_padded = pad_dataframe(farmers_df.copy(), all_columns)
facilities_padded = pad_dataframe(facilities_df.copy(), all_columns)
inventory_padded = pad_dataframe(inventory_df.copy(), all_columns)
batches_padded = pad_dataframe(batches_df.copy(), all_columns)
products_padded = pad_dataframe(products_df.copy(), all_columns)
orders_padded = pad_dataframe(orders_df.copy(), all_columns)

# 合并
complete_data = pd.concat([
    regions_padded,
    farmers_padded,
    facilities_padded,
    inventory_padded,
    batches_padded,
    products_padded,
    orders_padded
], ignore_index=True)

# 保存
output_file = 'flash_freeze_data.csv'
complete_data.to_csv(output_file, index=False, encoding='utf-8-sig')

print("\n" + "=" * 60)
print(f"✓ 完整数据已保存到: {output_file}")
print(f"  总记录数: {len(complete_data)}")
print("=" * 60)

# 数据统计摘要
print("\n数据统计摘要:")
print(f"  - 区域: {len(regions_df)}")
print(f"  - 农户: {len(farmers_df)}")
print(f"  - 冷冻设施: {len(facilities_df)}")
print(f"  - 农户库存: {len(inventory_df)}")
print(f"  - 冷冻批次: {len(batches_df)}")
print(f"  - 冷冻成品: {len(products_df)}")
print(f"  - 订单: {len(orders_df)}")
print("\n✓ 数据生成完成！可以运行 Shiny 应用了。\n")

