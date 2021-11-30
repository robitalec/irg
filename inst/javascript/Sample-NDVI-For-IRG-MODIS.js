// Sample NDVI for IRG - MODIS MOD13Q1
// Example from irg package https://github.com/robitalec/irg
// Alec L. Robitaille

// Functions ===================================================================
// Function to grab year from image and add it as a band
function addYear(img) {
  img.addBands(ee.Image(img.date().get('year')).rename('yr'));
}

// Function to sample an image in each region of supplied geometry
function sampleRegions (im) {
	return(im.reduceRegions(features, ee.Reducer.mean(), 250)
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
var modis = ee.ImageCollection('MODIS/006/MOD13Q1');



// Filter ======================================================================
modis = modis.filterDate('2018-01-01', '2021-08-01')
             .filterBounds(features);



// Process images ==============================================================
// Add dates
modis = modis.map(addDates);



// Sample images ===============================================================
var sample = modis.map(sampleRegions)
                  .flatten()
                  .filter(ee.Filter.neq('NDVI', null));



// Export ======================================================================
Export.table.toDrive({
  collection: sample,
  description: 'sampled-ndvi-modis-MOD13Q1'
});
