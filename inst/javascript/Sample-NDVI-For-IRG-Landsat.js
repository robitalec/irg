// Sample NDVI for IRG - Landsat
// Example from irg package https://github.com/robitalec/irg
// Alec L. Robitaille 

// Functions ===================================================================
// Function to add mask identifying cloud and saturation
// Adapted from: Examples/Cloud Masking/Landsat8 Surface Reflectance
function addMask(image) {
  // Bit 0 - Fill
  // Bit 1 - Dilated Cloud
  // Bit 2 - Cirrus
  // Bit 3 - Cloud
  // Bit 4 - Cloud Shadow
  var qaMask = image.select('QA_PIXEL').bitwiseAnd(parseInt('11111', 2)).eq(0);
  var saturationMask = image.select('QA_RADSAT').eq(0);
  var mask = qaMask.add(saturationMask);

  // Apply the scaling factors to the appropriate bands.
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);

  // Replace the original bands with the scaled ones and apply the masks.
  return image.addBands(opticalBands, null, true)
              .addBands(thermalBands, null, true)
              .addBands(mask.rename('mask'));
}

// Function to calculate NDVI and add it as a band
function calcNDVI(im) {
	var ndvi = im.normalizedDifference(['SR_B5','SR_B4']).rename('ndvi');
	return im.addBands(ndvi);
}

// Function to grab date from image and add it as a band
function addDates(img) {
  var date = img.date();
  return img.addBands(ee.Image([date.getRelative('day', 'year'),
                                date.get('year')])
                        .rename(['doy', 'year'])).float();
}

// Function to sample an image in each region of supplied geometry
function sampleRegions (im) {
	return(im.reduceRegions(features, ee.Reducer.mean(), 30)
           .copyProperties(im));
}



// Feature ====================================================================
var features = ee.Geometry.MultiPoint(
        [[-124.20936676431572, 56.0659251409004],
         [-124.14344879556572, 56.18227598886774],
         [-124.45106598306572, 56.243371999516576],
         [-124.24919220376884, 56.29598913650686],
         [-123.80012604165947, 56.07818917523702],
         [-123.96080109048759, 56.19297481990305],
         [-124.40849396158134, 56.1425113526211]]);



// Images ======================================================================
var l8 = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2');



// Filter ======================================================================
l8 = l8.filterDate('2015-01-01', '2020-01-01')
       .filterBounds(features);



// Process images ==============================================================
// Mask clouds and calculate NDVI
l8 = l8.map(addMask)
       .map(addDates)
       .map(calcNDVI);



// Sample images ===============================================================
var sample = l8.map(sampleRegions)
               .flatten()
               .filter(ee.Filter.neq('ndvi', null));



// Export ======================================================================
Export.table.toDrive({
  collection: sample,
  description: 'sampled-ndvi-Landsat-LC08-T1-L2'
});
