from pathlib import Path
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.ops import unary_union
from shapely.geometry import box
from matplotlib.patches import Patch

# ---------------- Paths ----------------
NE_ADMIN0 = Path("/Users/dastanjasim/Documents/kurd_cc/kurd_cc/data/raw/shapefile/ne_10m_admin_0_countries.shp")
NE_ADMIN1 = Path("/Users/dastanjasim/Documents/kurd_cc/kurd_cc/data/raw/shapefile/ne_10m_admin_1_states_provinces.shp")
LAYERS    = Path("/Users/dastanjasim/Documents/kurd_cc/kurd_cc/data/raw/shapefile/layers")
OUTDIR    = Path("/Users/dastanjasim/Documents/kurd_cc/kurd_cc/outputs/maps"); OUTDIR.mkdir(exist_ok=True)

# ---------------- Helpers ----------------
def read_gdf(p: Path) -> gpd.GeoDataFrame:
    g = gpd.read_file(p)
    if g.crs is None or g.crs.to_epsg() != 4326:
        g = g.set_crs(4326)
    return g

def _iso_col(gdf):
    for c in ("ADM0_A3","ISO_A3","SOV_A3","GU_A3","WB_A3","ADM0_A3_US"):
        if c in gdf.columns: return c
    return None

def _name_col(gdf):
    for c in ("NAME_EN","NAME_LONG","NAME","ADMIN","SOVEREIGNT"):
        if c in gdf.columns: return c
    return None

def setup_base():
    admin0 = read_gdf(NE_ADMIN0)
    iso_col = _iso_col(admin0)
    base = admin0[admin0[iso_col].isin(["TUR","IRN","IRQ","SYR"])].copy()
    admin1 = read_gdf(NE_ADMIN1)
    return base, admin1

def theme_bw(ax, region=None, pad_scale=(0.06, 0.12)):
    ax.set_axis_off()
    for s in ax.spines.values():
        s.set_visible(False)
    if region is not None and not region.empty:
        minx, miny, maxx, maxy = region.total_bounds
        padx, pady = (maxx-minx)*pad_scale[0], (maxy-miny)*pad_scale[1]
        ax.set_xlim(minx-padx, maxx+padx)
        ax.set_ylim(miny-pady, maxy+pady)

def _view_poly(ax):
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    return box(xmin, ymin, xmax, ymax)

def label_countries(ax, base, only_isos=None, overrides=None, name_overrides=None, clamp_inside=True):
    """
    Place country labels after zoom is set.
    - only_isos: subset list (e.g., ["IRN","IRQ"]); if None, label all base countries.
    - overrides: {"ISO": {"dx": float_deg, "dy": float_deg}}
    - name_overrides: {"ISO": "Custom Name"}
    - clamp_inside: keep text inside current axes extent.
    """
    overrides = overrides or {}
    name_overrides = name_overrides or {}
    iso_c  = _iso_col(base)
    name_c = _name_col(base)
    df = base if only_isos is None else base[base[iso_c].isin(only_isos)]
    view = _view_poly(ax)

    # label only countries that intersect the current view
    df = df[df.geometry.intersects(view)]

    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    pad_x = (xmax - xmin) * 0.015
    pad_y = (ymax - ymin) * 0.02

    for _, row in df.iterrows():
        try:
            iso = row[iso_c]
            pt = row.geometry.representative_point()
            x = pt.x + overrides.get(iso, {}).get("dx", 0.0)
            y = pt.y + overrides.get(iso, {}).get("dy", 0.0)

            if clamp_inside:
                x = max(min(x, xmax - pad_x), xmin + pad_x)
                y = max(min(y, ymax - pad_y), ymin + pad_y)

            label = name_overrides.get(iso) or (row[name_c] if name_c else None) or iso
            ax.annotate(
                str(label),
                xy=(x, y),
                ha="center", va="center",
                fontsize=9, color="black",
                bbox=dict(boxstyle="round,pad=0.2", facecolor="white", edgecolor="none", alpha=0.9)
            )
        except Exception:
            pass

# ---------------- 1) Kurdistan incl. exclaves ----------------
def map_kurdistan_full():
    base, admin1 = setup_base()
    main = read_gdf(LAYERS / "kurdistan_outline.geojson")
    excl = read_gdf(LAYERS / "kurdistan_exclaves.geojson")
    merged = unary_union([*main.geometry, *excl.geometry])
    region = gpd.GeoDataFrame(geometry=[merged], crs="EPSG:4326")

    fig, ax = plt.subplots(figsize=(12,5))
    base.plot(ax=ax, facecolor="0.90", edgecolor="black", linewidth=0.6, zorder=1)
    admin1.boundary.plot(ax=ax, color="0.7", linewidth=0.25, zorder=2)
    region.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=1.2, hatch="///", zorder=3)

    handles = [
        Patch(facecolor="0.90", edgecolor="black", label="Countries (base)"),
        Patch(facecolor="white", edgecolor="black", hatch="///", label="Kurdistan (incl. exclaves)")
    ]
    ax.legend(handles=handles, loc="lower left", frameon=True, edgecolor="black")

    theme_bw(ax, region)  # set extent first
    # Ensure IRAN visible
    label_countries(ax, base, only_isos=["TUR","IRN","IRQ","SYR"], overrides={"IRN": {"dy": 0.7}})

    fig.savefig(OUTDIR/"kurdistan_full_bw.pdf", bbox_inches="tight")
    plt.close(fig)

# ---------------- 2) KRI + Disputed ----------------
def map_kri_disputed():
    base, admin1 = setup_base()
    krg  = read_gdf(LAYERS / "krg_boundary.geojson")
    disp = read_gdf(LAYERS / "disputed_territories.geojson")

    region = krg.overlay(disp, how="union") if not krg.empty and not disp.empty else (krg if not krg.empty else disp)

    fig, ax = plt.subplots(figsize=(9,6))
    base.plot(ax=ax, facecolor="0.90", edgecolor="black", linewidth=0.6, zorder=1)
    admin1.boundary.plot(ax=ax, color="0.7", linewidth=0.25, zorder=2)

    # Disputed below, KRI on top (your “good” plot)
    disp.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=0.9, hatch="xxx", zorder=3)
    krg.plot(ax=ax,  facecolor="white", edgecolor="black", linewidth=1.2, hatch="///", zorder=4)

    # Guarantee boundaries visible
    disp.boundary.plot(ax=ax, color="black", linewidth=0.8, zorder=5)
    krg.boundary.plot(ax=ax,  color="black", linewidth=1.0, zorder=6)

    handles = [
        Patch(facecolor="white", edgecolor="black", hatch="///", label="KRI"),
        Patch(facecolor="white", edgecolor="black", hatch="xxx", label="Disputed territories")
    ]
    ax.legend(handles=handles, loc="lower left", frameon=True, edgecolor="black")

    theme_bw(ax, region)
    # Move IRAQ label up so it clears legend & overlays
    label_countries(ax, base, only_isos=["TUR","IRN","IRQ","SYR"], overrides={"IRQ": {"dy": 1.8}})

    fig.savefig(OUTDIR/"kri_disputed_bw.pdf", bbox_inches="tight")
    plt.close(fig)

# ---------------- 3) KRI + Disputed + Oil/Gas ----------------
def map_kri_resources():
    base, admin1 = setup_base()
    krg  = read_gdf(LAYERS / "krg_boundary.geojson")
    disp = read_gdf(LAYERS / "disputed_territories.geojson")
    oil  = read_gdf(LAYERS / "oil_fields.geojson")
    gas  = read_gdf(LAYERS / "gas_fields.geojson")

    region = krg.overlay(disp, how="union") if not krg.empty and not disp.empty else (krg if not krg.empty else disp)

    fig, ax = plt.subplots(figsize=(9,6))
    base.plot(ax=ax, facecolor="0.90", edgecolor="black", linewidth=0.6, zorder=1)
    admin1.boundary.plot(ax=ax, color="0.7", linewidth=0.25, zorder=2)

    # SAME layering/patterns as KRI+Disputed
    disp.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=0.9, hatch="xxx", zorder=3)
    krg.plot(ax=ax,  facecolor="white", edgecolor="black", linewidth=1.2, hatch="///", zorder=4)

    # Oil/Gas ON TOP with gray fills; use slight alpha so hatches remain visible in print.
    if not gas.empty:
        gas.plot(ax=ax, facecolor="0.80", edgecolor="black", linewidth=0.8, alpha=0.55, zorder=5)  # lighter gray
    if not oil.empty:
        oil.plot(ax=ax, facecolor="0.60", edgecolor="black", linewidth=0.8, alpha=0.65, zorder=6)  # darker gray

    # Re-stamp boundaries so overlays don't hide the shapes visually
    disp.boundary.plot(ax=ax, color="black", linewidth=0.9, zorder=7)
    krg.boundary.plot(ax=ax,  color="black", linewidth=1.1, zorder=8)

    handles = [
        Patch(facecolor="white", edgecolor="black", hatch="///", label="KRI"),
        Patch(facecolor="white", edgecolor="black", hatch="xxx", label="Disputed territories"),
        Patch(facecolor="0.80", edgecolor="black", label="Gas fields"),
        Patch(facecolor="0.60", edgecolor="black", label="Oil fields"),
    ]
    ax.legend(handles=handles, loc="lower left", frameon=True, edgecolor="black")

    theme_bw(ax, region)
    label_countries(ax, base, only_isos=["TUR","IRN","IRQ","SYR"], overrides={"IRQ": {"dy": 1.8}})

    fig.savefig(OUTDIR/"kri_resources_bw.pdf", bbox_inches="tight")
    plt.close(fig)

# ---------------- 4) AANES 2015 vs 2023 (+ Turkish control) ----------------
def map_aanes():
    base, admin1 = setup_base()
    nes15 = read_gdf(LAYERS / "nes_2015.geojson")
    nes23 = read_gdf(LAYERS / "nes_2023.geojson")

    def _has_tc(row): return any("TC" in str(v).upper() for v in row.values)
    tc = nes23[nes23.apply(_has_tc, axis=1)] if not nes23.empty else nes23
    nes23_clean = nes23.drop(tc.index) if not nes23.empty else nes23

    fig, ax = plt.subplots(figsize=(12, 6))
    base.plot(ax=ax, facecolor="0.90", edgecolor="black", linewidth=0.6, zorder=1)
    admin1.boundary.plot(ax=ax, color="0.7", linewidth=0.25, zorder=2)

    if not nes15.empty:
        nes15.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=1.0, hatch="...", zorder=3)
    if not nes23_clean.empty:
        nes23_clean.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=1.2, hatch="|||", zorder=4)
    if tc is not None and not tc.empty:
        tc.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=1.0, hatch="xx", zorder=5)

    handles = [
        Patch(facecolor="white", edgecolor="black", hatch="...", label="AANES 2015"),
        Patch(facecolor="white", edgecolor="black", hatch="|||", label="AANES 2023"),
        Patch(facecolor="white", edgecolor="black", hatch="xx",  label="Turkish control"),
    ]
    ax.legend(handles=handles, loc="lower left", frameon=True, edgecolor="black")

    # Zoom to SYRIA for more context
    iso_c = _iso_col(base); name_c = _name_col(base)
    if iso_c and not base[base[iso_c] == "SYR"].empty:
        syr = base[base[iso_c] == "SYR"]
    elif name_c and not base[base[name_c].str.contains("Syria", case=False, na=False)].empty:
        syr = base[base[name_c].str.contains("Syria", case=False, na=False)]
    else:
        syr = base
    theme_bw(ax, syr, pad_scale=(0.15, 0.20))

    # Labels: ensure TURKEY is present (and SYRIA)
    label_countries(ax, base, only_isos=["TUR","SYR"], overrides={"TUR": {"dy": 0.7}})

    fig.savefig(OUTDIR / "aanes_2015_2023_bw.pdf", bbox_inches="tight")
    plt.close(fig)

# ---------------- 5) Iran: Kurdish Sunni vs Shia ----------------
def map_iran_religion():
    base, admin1 = setup_base()
    sunni = read_gdf(LAYERS / "iran_kurdish_sunni.geojson")
    shia  = read_gdf(LAYERS / "iran_kurdish_shia.geojson")

    region = sunni.overlay(shia, how="union") if not sunni.empty and not shia.empty else (sunni if not sunni.empty else shia)

    fig, ax = plt.subplots(figsize=(6,9))
    base.plot(ax=ax, facecolor="0.90", edgecolor="black", linewidth=0.6, zorder=1)
    admin1.boundary.plot(ax=ax, color="0.7", linewidth=0.25, zorder=2)
    shia.plot(ax=ax,  facecolor="0.85", edgecolor="black", linewidth=0.9, zorder=3)
    sunni.plot(ax=ax, facecolor="white", edgecolor="black", linewidth=1.1, hatch="///", zorder=4)

    handles = [
        Patch(facecolor="white", edgecolor="black", hatch="///", label="Kurdish Sunni"),
        Patch(facecolor="0.85", edgecolor="black", label="Kurdish Shia")
    ]
    ax.legend(handles=handles, loc="lower left", frameon=True, edgecolor="black")

    # Zoom to region (Iran NW) then place labels for IRAN & IRAQ
    theme_bw(ax, region, pad_scale=(0.10, 0.14))
    label_countries(ax, base, only_isos=["IRN","IRQ"], overrides={"IRN": {"dy": 0.6}, "IRQ": {"dy": 0.6}})

    fig.savefig(OUTDIR/"iran_sunni_shia_bw.pdf", bbox_inches="tight")
    plt.close(fig)

# ---------------- Run all ----------------
if __name__ == "__main__":
    map_kurdistan_full()
    map_kri_disputed()
    map_kri_resources()
    map_aanes()
    map_iran_religion()
    print("✅ All maps exported to:", OUTDIR)
