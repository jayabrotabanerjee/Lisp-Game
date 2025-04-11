(define-quest :id "Q001"
               :name "Find the Lost Research Station"
               :description "Locate the abandoned research station on planet Delta-IV"
               :objectives '(("Find the station" :planet "Delta-IV"))
               :reward '((:credits 1000)
                        (:item "Advanced Scanner"))
               :story "A renowned scientist went missing on a remote planet.")

(define-quest :id "Q002"
               :name "Retrieve Alien Technology"
               :description "Obtain a piece of advanced tech from a Kraelion ship"
               :objectives '(("Encounter a Kraelion ship" :faction "Kraelion")
                            ("Retrieve the tech" :item "Alien Module"))
               :reward '((:credits 5000)
                        (:item "Alien Component"))
               :story "Stolen tech could give humanity an edge.")

(define-quest :id "Q003"
               :name "Escort Xenonian Diplomats"
               :description "Protect Xenonian diplomats en route to Andromeda"
               :objectives '(("Engage with Xenonian diplomats" :faction "Xenonians")
                            ("Reach destination safely" :system "Andromeda"))
               :reward '((:credits 2000)
                        (:item "Diplomatic Vest"))
               :story "Ensure safe passage for critical peace talks.")

(define-quest :id "Q004"
               :name "Destroy Pirate Base"
               :description "Eliminate the pirate base hidden in a nearby asteroid field"
               :objectives '(("Find the pirate base" :sector "Asteroid Field")
                            ("Destroy the base" :sector "Asteroid Field"))
               :reward '((:credits 3000)
                        (:item "Pirate's Treasure Map"))
               :story "Pirates have been terrorizing local space routes.")
