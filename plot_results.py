import json

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Set style
plt.style.use('default')
plt.rcParams['figure.figsize'] = [12, 8]
plt.rcParams['font.size'] = 10

# Read benchmark results
with open('benchmark_results.json', 'r') as f:
    data = json.load(f)

# Convert to DataFrame
df = pd.DataFrame([
    {
        'Language': lang,
        'Time': stats['mean'],
        'StdDev': stats['stddev'],
        'Min': stats['min'],
        'Max': stats['max'],
        'Group': 'System Languages' if lang in ['C', 'Rust'] 
                else 'JVM Languages' if lang in ['Java', 'Scala']
                else 'Functional Languages' if lang in ['Elixir', 'Clojure']
                else 'Other'
    }
    for lang, stats in data.items()
])

# Sort by execution time
df = df.sort_values('Time')

# Calculate relative performance
fastest_time = df['Time'].min()
df['Relative Performance'] = fastest_time / df['Time']

# Create figure with subplots
fig = plt.figure(figsize=(15, 12))
gs = plt.GridSpec(3, 2, height_ratios=[2, 1, 1])

# Color mapping for groups
colors = {
    'System Languages': '#1f77b4',
    'JVM Languages': '#2ca02c',
    'Functional Languages': '#ff7f0e',
    'Other': '#7f7f7f'
}

# Main performance plot
ax1 = plt.subplot(gs[0, :])
bars = ax1.bar(df.index, df['Time'], yerr=df['StdDev'], capsize=5)
for i, bar in enumerate(bars):
    bar.set_color(colors[df.iloc[i]['Group']])
    ax1.text(bar.get_x() + bar.get_width()/2., bar.get_height(),
             f'{df.iloc[i]["Time"]:.3f}s',
             ha='center', va='bottom')

ax1.set_title('Language Performance Comparison', pad=20)
ax1.set_xlabel('Programming Language')
ax1.set_ylabel('Execution Time (seconds)')
ax1.set_xticks(df.index)
ax1.set_xticklabels(df['Language'], rotation=45)
ax1.grid(True, linestyle='--', alpha=0.7)

# Relative performance plot
ax2 = plt.subplot(gs[1, 0])
bars = ax2.bar(df.index, df['Relative Performance'])
for i, bar in enumerate(bars):
    bar.set_color(colors[df.iloc[i]['Group']])
    ax2.text(bar.get_x() + bar.get_width()/2., bar.get_height(),
             f'{df.iloc[i]["Relative Performance"]:.2f}x',
             ha='center', va='bottom')

ax2.set_title('Relative Performance (higher is better)')
ax2.set_xlabel('Programming Language')
ax2.set_ylabel('Speed Relative to Fastest')
ax2.set_xticks(df.index)
ax2.set_xticklabels(df['Language'], rotation=45)
ax2.grid(True, linestyle='--', alpha=0.7)

# Group comparison
ax3 = plt.subplot(gs[1, 1])
group_stats = df.groupby('Group')['Time'].mean().sort_values()
bars = ax3.bar(range(len(group_stats)), group_stats)
for i, bar in enumerate(bars):
    bar.set_color(colors[group_stats.index[i]])
    ax3.text(bar.get_x() + bar.get_width()/2., bar.get_height(),
             f'{group_stats.iloc[i]:.3f}s',
             ha='center', va='bottom')

ax3.set_title('Average Performance by Language Group')
ax3.set_xlabel('Language Group')
ax3.set_ylabel('Mean Execution Time (seconds)')
ax3.set_xticks(range(len(group_stats)))
ax3.set_xticklabels(group_stats.index, rotation=45)
ax3.grid(True, linestyle='--', alpha=0.7)

# Statistics table
ax4 = plt.subplot(gs[2, :])
stats_df = pd.DataFrame({
    'Mean Time (s)': df['Time'].round(3),
    'Std Dev': df['StdDev'].round(3),
    'Min Time (s)': df['Min'].round(3),
    'Max Time (s)': df['Max'].round(3),
    'Relative Speed': df['Relative Performance'].round(2),
    'Group': df['Group']
})
stats_df.index = df['Language']

table = ax4.table(
    cellText=stats_df.values,
    rowLabels=stats_df.index,
    colLabels=stats_df.columns,
    cellLoc='center',
    loc='center',
    bbox=[0.1, 0.0, 0.8, 0.9]
)
ax4.axis('off')
table.auto_set_font_size(False)
table.set_fontsize(9)
table.scale(1.2, 1.5)

# Adjust layout
plt.tight_layout()

# Save plot with high DPI
plt.savefig('benchmark_results.png', dpi=300, bbox_inches='tight')
print("Plot saved as benchmark_results.png")
