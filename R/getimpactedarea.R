#' getimpactedarea
#' 
#' Performs 1) standardisation of reservoir extent, 2) calculation of river course upstream and downstream and 3) clipping to river basins
#' @export
#' @returns An sf multipolygon with the reservoir buffer, upstream and downstream areas
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param water_bodies A rast, where 1 indicates water, NA otherwise
#' @param dem A rast, showing elevation
#' @param fac A rast, showing accumulated water flow along river
#' @param pourpoints An sf multipoint, showing the points where rivers flow in and out of reservoirs
#' @param basins An sf multipolygon, with the basins in the area around the dam
#' @param tocrop A true/false parameter whether crop all input rasters by the river distance
#' @param toadjust A true/false parameter whether to adjust the reservoir to surrounding water bodies
#' @param poss_expand A number, indicating the number of meters away from the raw reservoir the reservoir may expand to. Default is 20000 (20km).
#' @param river_distance A number, indicating the number of meters downstream and upstream for the area of interest. Defaults to 100000 (100km)
#' @param nn A number, indicating the number of nearest neighbours to consider in the algorithm to determine river course. Higher can be more accurate but is slower. Default 100.
#' @param ac_tolerance A number, indicating the tolerance to changes in flow accumulation. Default 2, which means that if accumulated flow changes by a factor of 2 (halved or doubled) the area of interest should not include any further downstream or upstream. This is to account for confluences.
#' @param e_tolerance A number indicating the tolerance to changes in elevation. Rivers flow downstream. But DEMs can show downstream areas of the river as higher, due to averaging nearby pixels. This is particularly true when rivers run through gorges. If there is no downstream lower river poitn nearby, the elevation tolerance allows the algorithm to select a point at a higher elevation, up to the threshold defined here.
#' @param streambuffersize A number indicating the distance around the upstream and downstream river to consider as impacted. Defaults to 2000 (2km).
#' @param reservoirbuffersize A number indicating the distance around the reserviur to consider as impacted. Defaults to 5000 (5km)
#' @param wbjc A number, the water body join correction. This indicates the buffer zone for the reservoir, to ensure that it is contiguous (important where there are small channels connecting different parts of the same water body). Default is 0, but is necessary for some dams depending on the context. 


getimpactedarea <- function(
                          reservoir,
                          water_bodies,
                          dem,
                          fac,
                          basins,
                          pourpoints,
                          tocrop = TRUE,
                          toadjust = FALSE,
                          poss_expand = 20000,
                          river_distance = 100000,
                          nn = 100,
                          ac_tolerance = 2,
                          e_tolerance = 5,
                          streambuffersize = 2000,
                          reservoirbuffersize = 5000,
                          wbjc = 0) {
  
  # attempts to correct for invalid geometries for the input polygon
  reservoir <- reservoir %>% st_make_valid()
  if(tocrop == TRUE){
  cropped <- cropdata(reservoir = reservoir, 
                      dem = dem, fac = fac, water_bodies = water_bodies, basins = basins, 
                      river_distance = river_distance)
  dem <- cropped[[1]]; fc <- cropped[[2]]; wb <- cropped[[3]]; basins <- cropped[[4]]
  }
  if(toadjust == TRUE){
  # adjusts the surface area of the reservoir with satellite-observed surface water to ensure consistency.
  reservoir <- adjustreservoirpolygon(reservoir = reservoir, 
                                      water_bodies = water_bodies, 
                                      dem = dem,
                                      poss_expand = poss_expand,
                                      wbjc = wbjc)
  }
  # draws river points from pour points 

  ppids <- as.vector(1:nrow(pourpoints), mode = "list")
  riverpoints <-  lapply(
         X = ppids,
         FUN = getriverpoints,
         reservoir = reservoir,
         pourpoints = pourpoints,
         river_distance = river_distance,
         nn = nn,
         ac_tolerance = ac_tolerance,
         e_tolerance = e_tolerance,
         fac = fac,
         dem = dem)

  riverlines <- pointstolines(riverpoints = riverpoints)

  #creates buffers and clips to basins
  
  impactedarea <- basinandbuffers(reservoir = reservoir, 
                                  upstream = riverlines[[1]], 
                                  downstream = riverlines[[2]],
                                  basins = basins,
                                  streambuffersize = streambuffersize,
                                  reservoirbuffersize = reservoirbuffersize)
  
  # 'smooths' final polygon to remove jagged edges due to raster processing
  impactedarea <- smoothr::smooth(impactedarea[[2]], method = "ksmooth", smoothness = 3)
  return(impactedarea)
}
