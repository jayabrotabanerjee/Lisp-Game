(define-item :name "Basic Engine"
              :type :component
              :slot :engine
              :level 1
              :power-consumption 10
              :value 200
              :description "Default engine for most spaceships.")

(define-item :name "Light Laser"
              :type :component
              :slot :weapon
              :level 1
              :power-consumption 15
              :value 250
              :description "Basic laser cannon for defense.")

(define-item :name "Repair Kit"
              :type :consumable
              :value 50
              :effect '(:health 25)
              :description "Repairs 25 units of hull damage.")

(define-item :name "Fuel Cell"
              :type :consumable
              :value 30
              :effect '(:fuel 20)
              :description "Restores 20 units of fuel.")

(define-item :name "Medikit"
              :type :consumable
              :value 40
              :effect '(:health 50)
              :description "Restores 50 units of health.")

(define-item :name "Advanced Shield"
              :type :component
              :slot :shield
              :level 3
              :power-consumption 50
              :value 500
              :description "Significantly increases ship's defensive capabilities.")

(define-item :name "Plasma Cannon"
              :type :component
              :slot :weapon
              :level 4
              :power-consumption 60
              :value 800
              :description "Powerful weapon for engaging hostile aliens.")
