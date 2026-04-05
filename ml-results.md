# 1. What was your final model and why did you choose it over alternatives?

I chose the unregularized enhanced logistic regression which included demographics (age, sex, race, precinct, crime_suspected) and reasons flags and circumstance flags. This had a modest 7.4% improvement over the baseline. This was better than the simple model with demographics and pretty much identical to the regularized models. I chose the simpler unregularized model because no overfitting was detected.

# 2. How predictable are stop outcomes from the information available at the time of the stop? What does this tell us about policing?

Not very predictive! The variables with more predictive power were office's recorded reasons and circumstances noted and crime type suspected, rather than demographics. This suggests that stop outcomes are more influenced by street-level discretion by individual officers, rather than "who" the person is.

# 3. Should police departments use predictive models like this? What are the risks?

Predictive models like this should not be used because the risks outweigh the benefits. The models are not very predictive and relying on them can reinforce algorithmic bias hidden in the training data. The risk of wrongly arresting or over-policing outweighs the benefits of increased convenience of using these models. 
