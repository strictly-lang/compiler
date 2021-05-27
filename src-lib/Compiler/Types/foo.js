
(() => {
	class TestComponentsHelperMatchBase extends HTMLElement {
		constructor() {
			super();
			this._mounted = false;
			this._properties = {};
		}
		
		data(updateCallback) {
			const __init = () => {
				return { _type: "First"}
			}
			const __reducer = (_arg0, _arg1) => {
				if( _arg0._type == "First") {
					return { _type: "Second"}				
				}
				if( _arg0._type == "Second") {
					return { _type: "Third"}				
				}
			}
			
			const result = [
				__init(),
				(action) => {
					const reducerResult = __reducer(result[0], action)
					if (Object.is(reducerResult, result[0]) === false) {
						result[0] = reducerResult
						updateCallback();
					}
				}
			];
			
			return result
		}
		
		connectedCallback() {
			this._mounted = true;
			this._el = {};
			this.attachShadow({mode: 'open'});
			this._el.el0 = document.createElement("div");
			this.shadowRoot.prepend(this._el.el0);
			this._el.model1 = this.data(() => {
							
			})
			this._el.currentValue2 = undefined;
			this._el.currentCase2 = undefined;
			this._el.updateCallback2 = () => {
				const previousValue = this._el.currentValue2;
				const previousCase = this._el.currentCase2;
				this._el.currentValue2 = this._el.model1[0]
				this._el.currentCase2 = 
					this._el.currentValue2._type == "First" ? 0 : 
					this._el.currentValue2._type == "Second" ? 1 : 
					this._el.currentValue2._type == "Third" ? 2 : 
					(() => {throw new Error("No matching pattern found")()				;
			
			}
		}
		
	}
	
	customElements.define("test-components-helper-match-base", TestComponentsHelperMatchBase);
})()

