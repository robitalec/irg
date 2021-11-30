// Sample NDVI for IRG - MODIS MOD13Q1
// Example from irg package https://github.com/robitalec/irg
// Alec L. Robitaille

// Functions ===================================================================
// Function to grab year from image and add it as a band
function addYear(img) {
  return(img.addBands(ee.Image(img.date().get('year')).rename('yr')));
}

// Function to sample an image in each region of supplied geometry
function sampleRegions (im) {
	return(im.reduceRegions(features, ee.Reducer.mean(), 250)
           .copyProperties(im));
}



// Feature ====================================================================
var features = ee.FeatureCollection(
        [ee.Feature(ee.Geometry.Point([-109.96, 53.853]),
        {"id": "0"}),
        ee.Feature(ee.Geometry.Point([-109.94, 53.852]),
        {"id": "1"}),
        ee.Feature(ee.Geometry.Point([-109.95, 53.851]),
        {"id": "2"}),
        ee.Feature(ee.Geometry.Point([-109.93, 53.854]),
        {"id": "3"}),
        ee.Feature(ee.Geometry.Point([-109.92, 53.853]),
        {"id": "4"}),
        ee.Feature(ee.Geometry.Point([-109.94, 53.852]),
        {"id": "5"}),
        ee.Feature(ee.Geometry.Point([-109.93, 53.851]),
        {"id": "6"})]);


// Images ======================================================================
var modis = ee.ImageCollection('MODIS/006/MOD13Q1');



// Filter ======================================================================
modis = modis.filterDate('2015-01-01', '2020-01-01')
             .filterBounds(features);



// Process images ==============================================================
// Add dates
modis = modis.map(addYear);



// Sample images ===============================================================
var sample = modis.map(sampleRegions)
                  .flatten()
                  .filter(ee.Filter.neq('NDVI', null));



// Export ======================================================================
Export.table.toDrive({
  collection: sample,
  description: 'sampled-ndvi-MODIS-MOD13Q1'
});
