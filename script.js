const stateFipsToAbbreviation = {
    "01": "AL", "02": "AK", "04": "AZ", "05": "AR", "06": "CA", "08": "CO",
    "09": "CT", "10": "DE", "11": "DC", "12": "FL", "13": "GA", "15": "HI",
    "16": "ID", "17": "IL", "18": "IN", "19": "IA", "20": "KS", "21": "KY",
    "22": "LA", "23": "ME", "24": "MD", "25": "MA", "26": "MI", "27": "MN",
    "28": "MS", "29": "MO", "30": "MT", "31": "NE", "32": "NV", "33": "NH",
    "34": "NJ", "35": "NM", "36": "NY", "37": "NC", "38": "ND", "39": "OH",
    "40": "OK", "41": "OR", "42": "PA", "44": "RI", "45": "SC", "46": "SD",
    "47": "TN", "48": "TX", "49": "UT", "50": "VT", "51": "VA", "53": "WA",
    "54": "WV", "55": "WI", "56": "WY", "72": "PR"
};

document.addEventListener('DOMContentLoaded', function () {
    const riskButtons = document.querySelectorAll('.risk-button');
    const calcButtons = document.querySelectorAll('.calc-button');
    const calculateBtn = document.getElementById('calculate-btn');
    const countySelect = document.getElementById('county-select');
    const buildYearInput = document.getElementById('build-year');
    const lifespanInput = document.getElementById('lifespan');
    const shadedCountDisplay = document.createElement('div');

    shadedCountDisplay.id = 'shaded-count-display';
    shadedCountDisplay.style.padding = '10px';
    shadedCountDisplay.style.fontSize = '16px';
    shadedCountDisplay.style.backgroundColor = '#f9f9f9';
    shadedCountDisplay.style.borderTop = '1px solid #ddd';
    shadedCountDisplay.innerHTML = 'Shaded Counties: 0';
    document.querySelector('.sidebar-footer').appendChild(shadedCountDisplay);

    let csvData = [];
    let currentHighlightedLayer = null;
    let shadedCountyCount = 0;
    let shadedBounds = L.latLngBounds();

    riskButtons.forEach(button => {
        button.addEventListener('click', function () {
            riskButtons.forEach(btn => btn.classList.remove('active'));
            this.classList.add('active');
        });
    });

    calcButtons.forEach(button => {
        button.addEventListener('click', function () {
            calcButtons.forEach(btn => btn.classList.remove('active'));
            this.classList.add('active');
        });
    });

    function updateMap() {
        const riskCategory = document.querySelector('.risk-button.active').getAttribute('data-value');
        const buildYear = document.getElementById('build-year').value;
        const lifespan = document.getElementById('lifespan').value;
        const calcMethod = document.querySelector('.calc-button.active').getAttribute('data-value');

        if (!buildYear || !lifespan || !calcMethod || !riskCategory) {
            alert("Please fill in all the fields.");
            return;
        }

        let csvFilename;
        if (calcMethod === 'LEP-AEP') {
            csvFilename = `Data/US_counties_Gori200_${buildYear}_${lifespan}_AEP${riskCategory}.csv`;
        } else {
            csvFilename = `Data/US_counties_Gori200_${buildYear}_${lifespan}_${calcMethod}${riskCategory}.csv`;
        }

        console.log(`Fetching CSV file from: ${csvFilename}`);

        fetch(csvFilename)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error! Status: ${response.status}`);
                }
                return response.text();
            })
            .then(data => {
                Papa.parse(data, {
                    header: true,
                    complete: function (results) {
                        csvData = results.data;
                        console.log('CSV Data Loaded:', csvData);
                        applyMapData();
                    }
                });
            })
            .catch(error => {
                console.error('Error loading the CSV file:', error);
                alert("Failed to load the CSV file. Please check the file path and try again.");
            });
    }

    function applyMapData() {
        shadedCountyCount = 0;
        shadedBounds = L.latLngBounds();
        const unmatchedEntries = [];

        geojson.eachLayer(function (layer) {
            const geojsonCountyID = layer.feature.properties.STATEFP.padStart(2, '0') + layer.feature.properties.COUNTYFP.padStart(3, '0');

            const countyData = csvData.find(row => row.ID.padStart(5, '0') === geojsonCountyID);

            if (countyData) {
                const xdValue = parseFloat(countyData.XD);

                if (xdValue !== undefined) {
                    layer.setStyle({
                        fillColor: getColor(xdValue),
                        fillOpacity: 0.7,
                        color: '#C5C5C5',
                        weight: 1
                    });
                    layer.feature.properties.fillColor = getColor(xdValue);
                    layer.feature.properties.xdValue = xdValue;
                    layer.bindTooltip(`County: ${layer.feature.properties.NAME}<br>Wind speed: ${xdValue} mph`, {
                        permanent: false,
                        direction: 'top',
                        className: 'county-tooltip',
                        offset: [0, -10]
                    });
                    shadedCountyCount++;
                    shadedBounds.extend(layer.getBounds());
                } else {
                    console.log('No Data for County:', layer.feature.properties.NAME);
                    unmatchedEntries.push(layer.feature.properties.NAME);
                    layer.setStyle({
                        fillColor: '#FFFFFF',
                        fillOpacity: 0,
                        color: '#10101010',
                        weight: 1
                    });
                    layer.feature.properties.fillColor = '#FFFFFF';
                    layer.feature.properties.xdValue = null;
                    layer.bindTooltip(`County: ${layer.feature.properties.NAME}<br>No data available`, {
                        permanent: false,
                        direction: 'top',
                        className: 'county-tooltip',
                        offset: [0, -10]
                    });
                }
            }
        });

        shadedCountDisplay.innerHTML = `Shaded Counties: ${shadedCountyCount}`;
        console.log(`Total Shaded Counties: ${shadedCountyCount}`);
        console.log('Unmatched Entries:', unmatchedEntries);
    }

    function getColor(value) {
        const minValue = 0;
        const maxValue = 150;
        const scale = d3.scaleLinear()
            .domain([minValue, maxValue])
            .range(['#00BFFF', '#800080']);
        return scale(value);
    }

    calculateBtn.addEventListener('click', updateMap);

    buildYearInput.addEventListener('change', function () {
        if (buildYearInput.value) {
            buildYearInput.classList.add('selected');
        }
    });

    lifespanInput.addEventListener('change', function () {
        if (lifespanInput.value) {
            lifespanInput.classList.add('selected');
        }
    });

    var map = L.map('map').setView([37.8, -96], 4);

    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);

    function style(feature) {
        return {
            color: '#10101010',
            weight: 1,
            fillColor: feature.properties.fillColor || '#FFFFFF',
            fillOpacity: feature.properties.fillColor ? 0.7 : 0
        };
    }

    function highlightFeature(e) {
        var layer = e.target;

        layer._originalStyle = {
            weight: layer.options.weight,
            color: layer.options.color,
            fillOpacity: layer.options.fillOpacity,
            fillColor: layer.options.fillColor
        };

        layer.setStyle({
            weight: 2,
            color: '#383434',
            fillOpacity: 0.7,
            fillColor: '000000'
        });

        if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
            layer.bringToFront();
        }

        const stateFips = layer.feature.properties.STATEFP.padStart(2, '0');
        const stateAbbr = stateFipsToAbbreviation[stateFips];
        const countyName = layer.feature.properties.NAME;
        const tooltipContent = layer.feature.properties.xdValue !== null ?
            `County: ${countyName}, ${stateAbbr}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
            `County: ${countyName}, ${stateAbbr}`;
        layer.bindTooltip(tooltipContent, {
            permanent: false,
            direction: 'top',
            className: 'county-tooltip',
            offset: [0, -10]
        }).openTooltip();
    }

    function resetHighlight(e) {
        var layer = e.target;
        layer.setStyle(layer._originalStyle);
        layer.closeTooltip();
        const stateFips = layer.feature.properties.STATEFP.padStart(2, '0');
        const stateAbbr = stateFipsToAbbreviation[stateFips];
        const countyName = layer.feature.properties.NAME;
        const tooltipContent = layer.feature.properties.xdValue !== null ?
            `County: ${countyName}, ${stateAbbr}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
            `County: ${countyName}, ${stateAbbr}`;
        layer.bindTooltip(tooltipContent, {
            permanent: false,
            direction: 'top',
            className: 'county-tooltip',
            offset: [0, -10]
        });
    }

    function highlightOnClick(e) {
        var layer = e.target;
        if (currentHighlightedLayer) {
            geojson.resetStyle(currentHighlightedLayer);
            currentHighlightedLayer.closeTooltip();
        }
        currentHighlightedLayer = layer;
        layer.setStyle({
            weight: 2,
            color: '#383434',
            fillOpacity: 0.9,
            fillColor: layer.feature.properties.fillColor
        });
        map.flyToBounds(layer.getBounds(), {
            padding: [80, 80],
            duration: 0.8
        });
        const tooltipContent = layer.feature.properties.xdValue !== null ?
            `County: ${layer.feature.properties.NAME}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
            `County: ${layer.feature.properties.NAME}`;
        layer.bindTooltip(tooltipContent, {
            permanent: false,
            direction: 'top',
            className: 'county-tooltip',
            offset: [0, -10]
        }).openTooltip();
    }

    function onEachFeature(feature, layer) {
        const stateFips = feature.properties.STATEFP.padStart(2, '0');
        const stateAbbr = stateFipsToAbbreviation[stateFips];
        const countyName = feature.properties.NAME;
        layer.on({
            mouseover: highlightFeature,
            mousemove: function (e) {
                const tooltipContent = layer.feature.properties.xdValue !== null ?
                    `${countyName} County, ${stateAbbr}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
                    `${countyName} County, ${stateAbbr}`;
                layer.setTooltipContent(tooltipContent);
            },
            mouseout: resetHighlight,
            click: highlightOnClick
        });
        const initialTooltipContent = layer.feature.properties.xdValue !== null ?
            `${countyName} County, ${stateAbbr}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
            `${countyName} County, ${stateAbbr}`;
        layer.bindTooltip(initialTooltipContent, {
            permanent: false,
            direction: 'top',
            className: 'county-tooltip',
            offset: [0, -10]
        });
    }

    function populateCountyDropdown() {
        countySelect.innerHTML = '<option value="" disabled selected>Select...</option>';

        fetch('/Data/US_counties_Gori200_2030_30_LEP1.csv')
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error! Status: ${response.status}`);
                }
                return response.text();
            })
            .then(data => {
                Papa.parse(data, {
                    header: true,
                    complete: function (results) {
                        const counties = results.data;
                        counties.forEach(row => {
                            const countyName = row.NAME;
                            const stateFips = row.STATEFP.padStart(2, '0');
                            const stateAbbr = stateFipsToAbbreviation[stateFips];
                            const option = document.createElement('option');
                            option.value = `${countyName}, ${stateAbbr}`;
                            option.text = `${countyName}, ${stateAbbr}`;
                            countySelect.add(option);
                        });
                    }
                });
            })
            .catch(error => {
                console.error('Error loading the CSV file for county dropdown:', error);
            });
    }
    var geojson;
    fetch('usa-counties.geojson')
        .then(response => response.json())
        .then(data => {
            geojson = L.geoJSON(data, {
                style: style,
                onEachFeature: onEachFeature
            }).addTo(map);
            populateCountyDropdown(data.features);
        })
        .catch(error => console.error('Error loading the GeoJSON file:', error));

    fetch('us-states.geojson')
        .then(response => response.json())
        .then(data => {
            L.geoJSON(data, {
                style: function (feature) {
                    return {
                        color: '#aaaaaa',
                        weight: 1.5,
                        fillOpacity: 0
                    };
                }
            }).addTo(map);
        })
        .catch(error => console.error('Error loading the GeoJSON file:', error));

    L.Control.Home = L.Control.extend({
        onAdd: function (map) {
            var btn = L.DomUtil.create('button', 'leaflet-control-home');
            btn.innerHTML = 'Rescale';
            btn.onclick = function () {
                if (shadedCountyCount > 0) {
                    map.flyToBounds(shadedBounds, {
                        padding: [80, 80],
                        duration: 1
                    });
                } else {
                    map.flyTo([37.8, -96], 4, {
                        duration: 1
                    });
                }
                if (currentHighlightedLayer) {
                    geojson.resetStyle(currentHighlightedLayer);
                    currentHighlightedLayer.closeTooltip();
                    currentHighlightedLayer = null;
                }
            };
            return btn;
        },
        onRemove: function (map) {}
    });

    L.control.home = function (opts) {
        return new L.Control.Home(opts);
    };

    L.control.home({ position: 'bottomleft' }).addTo(map);

    countySelect.addEventListener('change', function () {
        const selectedCounty = countySelect.value;
        geojson.eachLayer(function (layer) {
            if (`${layer.feature.properties.NAME}, ${stateFipsToAbbreviation[layer.feature.properties.STATEFP.padStart(2, '0')]}` === selectedCounty) {
                if (currentHighlightedLayer) {
                    geojson.resetStyle(currentHighlightedLayer);
                    currentHighlightedLayer.closeTooltip();
                }
                currentHighlightedLayer = layer;
                layer.setStyle({
                    weight: 2,
                    color: '#383434',
                    fillOpacity: 0.9,
                    fillColor: layer.feature.properties.fillColor
                });
                map.flyToBounds(layer.getBounds(), {
                    padding: [80, 80],
                    duration: 0.8
                });
                const tooltipContent = layer.feature.properties.xdValue !== null ?
                    `County: ${layer.feature.properties.NAME}<br>Wind speed: ${layer.feature.properties.xdValue} mph` :
                    `County: ${layer.feature.properties.NAME}`;
                layer.bindTooltip(tooltipContent, {
                    permanent: false,
                    direction: 'top',
                    className: 'county-tooltip',
                    offset: [0, -10]
                }).openTooltip();
            }
        });
    });

    var legend = L.control({ position: 'bottomright' });

    legend.onAdd = function (map) {
        var div = L.DomUtil.create('div', 'info legend'),
            grades = [0, 30, 60, 90, 120, 150];

        var gradient = '<i style="background: linear-gradient(to right, #00BFFF, #800080); width: 100%; height: 10px; display: block;"></i>';

        var labels = [gradient];

        for (var i = 0; i < grades.length; i++) {
            labels.push(
                '<span style="float: left; margin-right: 16px;">' + grades[i] + '</span>');
        }

        labels.push('<span style="float: right;">mph</span>');

        div.innerHTML = labels.join('');
        return div;
    };

    legend.addTo(map);
});
