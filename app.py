import time

from shiny import reactive
from shiny.express import render, ui, input
from shinywidgets import render_widget
import ipyleaflet as L
import pandas as pd
# Add this import
from ipywidgets import HTML

from shared import load_data

# Stores the ID of the marker
clicked_marker_id = reactive.Value(0)

ui.page_opts(title="Pantry Maps", fillable=True)


# 2. Add JavaScript to fetch the location and update the input
# The JS will run as soon as the map loads.
ui.HTML("""
    <script type="text/javascript">
        // Function to get the current position
        function getLocation() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(showPosition, handleError);
            } else {
                console.log("Geolocation is not supported by this browser.");
            }
        }

        // Success callback: update the Shiny input
        function showPosition(position) {
            const lat = position.coords.latitude;
            const lon = position.coords.longitude;
            // Format as a string 'lat,lon'
            Shiny.setInputValue("current_location", `${lat},${lon}`, {priority: "event"});
        }

        // Error callback
        function handleError(error) {
            let error_message = "";
            switch(error.code) {
                case error.PERMISSION_DENIED:
                    error_message = "User denied the request for Geolocation.";
                    break;
                case error.POSITION_UNAVAILABLE:
                    error_message = "Location information is unavailable.";
                    break;
                case error.TIMEOUT:
                    error_message = "The request to get user location timed out.";
                    break;
                case error.UNKNOWN_ERROR:
                    error_message = "An unknown error occurred.";
                    break;
            }
            console.error("Geolocation Error: " + error_message);
            // Optionally set the input to an error state if needed
            // Shiny.setInputValue("current_location", "ERROR", {priority: "event"});
        }

        // Run on page load
        getLocation();
    </script>
""")

pantries = load_data()

# Variable to store the map widget instance so we can modify it later
map_widget = None
# Variable to store the current location marker
user_marker = L.CircleMarker(location=(0, 0), radius=10, color='white', fill_color='blue', fill_opacity=0.8)

with ui.layout_columns(col_widths=(8, 4)):
    @render_widget
    def leaflet_map():
        global map_widget
        m = L.Map(zoom=11, center=(47.60, -122.30))

        time.sleep(3)
        for _, row in pantries.iterrows():
            lat, lon = row['LAT'], row['LON']
            if pd.notna(lat) and pd.notna(lon):
                if row['exists_short'] == 'DONT KNOW':
                    icon = L.AwesomeIcon(name='circle', marker_color='gray', icon_color='white')
                elif row['exists_short'] == 'YES':
                    icon = L.AwesomeIcon(name='circle', marker_color='green', icon_color='white')
                else:
                    icon = L.AwesomeIcon(name='circle', marker_color='red', icon_color='white')

                marker = L.Marker(location=(lat, lon), icon=icon, draggable=False)

                # Bind the current row's ID into the callback
                def on_click(*, marker_id=row["ID"], **kwargs):
                    clicked_marker_id.set(marker_id)

                marker.on_click(on_click)
                m.add(marker)

        # Add the user marker to the map, initially at (0,0)
        m.add(user_marker)
        # 4. Add the legend to the map
        m.add(
            L.LegendControl(
                title="Pantry Status",
                legend={
                    "Unknown": 'gray', "Validated":"green", "Doesn't Exist":"red"
                },
                position="topright",
                # Style the legend to look cleaner
                max_height="200px",
                overflow_y="auto"
            )
        )

        map_widget = m
        return m


    # 3. Reactive effect to update the marker when the location input changes
    @reactive.effect
    def update_user_location():
        global map_widget, user_marker
        location_str = input.current_location()

        if location_str and location_str != "ERROR" and map_widget is not None:
            try:
                # Parse the 'lat,lon' string
                lat, lon = map(float, location_str.split(','))

                # Update the marker's location trait
                user_marker.location = (lat, lon)

                # # Create a simple popup for the user
                # user_marker.popup = L.Popup(
                #     location=(lat, lon),
                #     child=popup_content,
                #     close_button=False
                # )

                # Optionally, center the map on the user's location the first time
                # Note: 'User Location' is an arbitrary string label for the input
                if not reactive.get_user_location.is_set():
                    map_widget.center = (lat, lon)
                    map_widget.zoom = 14
                    reactive.get_user_location.set(True)  # Set a flag to prevent re-centering

            except ValueError:
                # Handle cases where the string isn't in the expected format
                pass


    # Removed: pantries = load_data() # Already done before layout_columns

    with ui.card():
        @render.data_frame
        def code():
            return render.DataGrid(
                pantries.loc[
                    pantries['ID'] == int(clicked_marker_id()),
                    ['ID', 'ADDRESS', 'exists_short']
                ],
                height='150px'
            )


        with ui.card():
            ui.h5('Actions')

            # 1. Replace ui.input_action_button with ui.a() for the link
            ui.a(
                'Report a New Pantry',  # The text displayed on the button
                href='https://forms.gle/S1UWK8DHKfnvo8ju7',
                target="_blank",  # Opens link in a new tab
                class_='btn btn-info w-100 mb-2',  # Style as a blue, full-width button
                id='new_pantry'  # Optional ID
            )


            @render.ui
            def render_button():
                return ui.a(
                    'Validate a Pantry',  # The text displayed on the button
                    href='https://forms.gle/53esBcEP44NMY3dy7',
                    target="_blank",  # Opens link in a new tab
                    class_='btn btn-info w-100 mb-2',  # Style as a blue, full-width button
                    id='val_pantry'  # Optional ID
                )


            # 1. Add a hidden input to store the location from JavaScript
            ui.input_text("current_location", value="", label=None, width="0px")

# Reactive value to track if the initial centering has happened
reactive.get_user_location = reactive.Value(False)